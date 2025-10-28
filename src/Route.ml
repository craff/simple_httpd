open Response_code
exception Pass (* raised to test the next handler *)
let pass () = raise Pass

type path = string list (* split on '/' *)

type (_, _) comp =
  | Exact : string -> ('a, 'a) comp
  | Int : (int -> 'a, 'a) comp
  | String : (string -> 'a, 'a) comp

type (_, _) t =
  | Fire : ('b, 'b) t
  | Rest : (string list -> 'b, 'b) t
  | Compose: ('a, 'b) comp * ('b, 'c) t -> ('a, 'c) t

let bpf = Printf.bprintf
let rec pp_
        : type a b. Buffer.t -> (a,b) t -> unit
  = fun out -> function
            | Fire -> bpf out "/"
            | Rest  -> bpf out "<rest_of_url>"
            | Compose (Exact s, tl) -> bpf out "%s/%a" s pp_ tl
            | Compose (Int, tl) -> bpf out "<int>/%a" pp_ tl
            | Compose (String, tl) -> bpf out "<str>/%a" pp_ tl

let to_string x =
  let b = Buffer.create 16 in
  pp_ b x;
  Buffer.contents b
let pp out x = Format.pp_print_string out (to_string x)

let return = Fire
let rest   = Rest
let (@/) a b = Compose (a,b)
let string = String
let int = Int
let exact (s:string) = Exact s
let exact_path (s:string) tail =
  let rec fn = function
    | [] -> tail
    | ""::ls -> fn ls
    | s::ls -> exact s @/ fn ls
  in
  fn (String.split_on_char '/' s)

type 'a cell = C : (('b,'c) t * 'a) -> 'a cell

type 'a otree =
  { exact : (string, 'a otree) Hashtbl.t
  ; mutable others : 'a cell list }

let empty_otree () = { exact = Hashtbl.create 16; others = [] }

type 'a tree =
  { get : 'a otree
  ; put : 'a otree
  ; post : 'a otree
  ; delete : 'a otree
  ; options : 'a otree
  ; connect : 'a otree
  ; trace : 'a otree }

let empty_tree () =
  { get = empty_otree ()
  ; put = empty_otree ()
  ; post = empty_otree ()
  ; delete = empty_otree ()
  ; options = empty_otree ()
  ; connect = empty_otree ()
  ; trace = empty_otree () }

let rec compare : type a1 a2 b1 b2.(a1,b1) t -> (a2,b2) t -> int = fun r1 r2 ->
  match (r1, r2) with
  | (Fire, Fire) -> 0
  | (Fire, _) -> -1
  | (_, Fire) -> 1
  | (Compose(Exact _, p), Compose(Exact _, q)) -> compare p q
  | (Compose(Exact _, _), _) -> -1
  | (_, Compose(Exact _, _)) -> 1
  | (Compose(Int, p), Compose(Int, q)) -> compare p q
  | (Compose(Int, _), _) -> -1
  | (_, Compose(Int, _)) -> 1
  | (Compose(String, p), Compose(String, q)) -> compare p q
  | (Compose(String, _), _) -> -1
  | (_, Compose(String, _)) -> 1
  | (Rest, Rest) -> 0

let insert_list : 'a cell list -> ('b,'c) t -> (('b,'c) t -> 'a) -> 'a cell list =
  fun l p f ->
  let cell = C(p,f p) in
  let rec fn l =
    match l with
    | [] -> [cell]
    | (C(q,_) as c :: l') ->
       if compare p q < 0 then
         cell :: l
       else
         c :: fn l'
  in fn l

let insert : type a b c.Method.t -> (a,b) t -> c tree -> ((a,b) t -> c) -> unit =
  fun m p t x ->
  let t =
    let open Method in
    match m with
    | GET -> t.get
    | PUT -> t.put
    | POST -> t.post
    | HEAD -> t.get
    | DELETE -> t.delete
    | OPTIONS -> t.options
    | CONNECT -> t.connect
    | TRACE -> t.trace
  in
  let rec fn : c otree -> (a,b) t -> unit =
    fun t -> function
          | Compose(Exact s, p) ->
             let t =
               try Hashtbl.find t.exact s
               with Not_found ->
                 let t' = empty_otree () in
                 Hashtbl.add t.exact s t';
                 t'
             in
             fn t p
          | p ->
             t.others <- insert_list t.others p x
  in
  fn t p

let get : Method.t -> string list -> 'c tree -> string list * 'c cell list =
  fun m p t ->
  let open Method in
  let t = match m with
    | GET -> t.get
    | PUT -> t.put
    | POST -> t.post
    | HEAD -> t.get
    | DELETE -> t.delete
    | OPTIONS -> t.options
    | CONNECT -> t.connect
    | TRACE -> t.trace
  in
  let rec fn t = function
    | [] -> ([], t.others)
    | s::q as p ->
       try fn (Hashtbl.find t.exact s) q
       with Not_found -> (p, t.others)
  in
  fn t p

let rec eval :
          type a b. path -> (a,b) t -> a -> b =
  fun path route f ->
  begin match path, route with
  | [], Fire -> f
  | _, Fire -> pass ()
  | path, Rest -> f path
  | [], Compose _ -> pass ()
  | x::path, Compose (ty, route) ->
     match (ty,route) with
     | (Exact s,route) -> if x <> s then pass () else eval path route f
     | (Int,    route) -> (try let x = int_of_string x in eval path route (f x)
                           with _ -> pass ())
     | (String, route) -> eval path route (f x)
  end

type 'a treatment = Output.t ->
                    Input.t Request.t ->
                    resp:(Response.t -> unit) -> 'a

module Filter = struct
  type 'a t = 'a Request.t -> 'a Request.t * (Response.t -> Response.t)

  let decode_request : ('a -> 'a) -> (Headers.t -> Headers.t) -> 'a t =
    (* turn it into a middleware *)
    fun tb th req ->
    let open Request in
    (* see if [f] modifies the stream *)
    ({req with body = tb req.body; headers = th req.headers }, fun r -> r)

  let encode_response : (Response.body -> Response.body) ->
                        (Headers.t -> Headers.t) -> 'a t =
    fun tb th req ->
    (req, fun resp ->
          let open Response in
          { resp with body = tb resp.body; headers = th resp.headers })

  let idt req = (req, (fun x -> x))

  let compose_embrace : 'a t -> 'a t -> 'a t =
    fun f1 f2 req ->
    let (req, f2) = f2 req in
    let (req, f1) = f1 req in
    (req, fun resp -> f1 (f2 resp))

  let compose_cross : 'a t -> 'a t -> 'a t =
    fun f1 f2 req ->
    let (req, f2) = f2 req in
    let (req, f1) = f1 req in
    (req, fun resp -> f2 (f1 resp))
end

type path_handler = Input.t Filter.t * (path -> unit treatment)

type handler = path_handler tree * (string, path_handler tree) Hashtbl.t

type handlers = handler array

(* route the given handler.
   @param tr_req wraps the actual concrete function returned by the route
   and makes it into a handler. *)
let add_route_handler
      ?addresses ?meth
      ?(filter=(fun x -> (x, fun x -> x) : Input.t Filter.t))
      ~(tr_req : 'a treatment) (handlers : handlers) route f =
  let fn route =
    let ph path =
      let f = eval path route f in
      fun oc req ~resp -> tr_req oc req ~resp f
    in
    (filter, ph)
  in
  let gn t =
    match meth with
    | Some m ->
       insert m route t fn
    | None ->
       insert GET route t fn;
       insert POST route t fn
  in
  let kn hosts (default, specific) = match hosts with
    | [] -> gn default
    | l  ->
       List.iter (fun h ->
           let tree =
             try Hashtbl.find specific h
             with Not_found ->
                   let t = empty_tree () in
                   Hashtbl.add specific h t;
                   t
           in
           gn tree) l
  in

  match addresses with
  | None -> Array.iter (kn []) handlers
  | Some l ->
     List.iter Address.(fun a -> kn a.hosts handlers.((index a :> int))) l

let empty_handler _ = (empty_tree (), Hashtbl.create 16)

let find (handlers : handlers) (req : Input.t Request.t) =
  let (default, specific) = handlers.(req.client.Async.accept_by) in
  let trees =
    if Hashtbl.length specific <= 0 then [default] else
      let host = Headers.(get Host req.Request.headers) in
      match host with
      | None -> [default]
      | Some host ->
         match String.split_on_char ':' host with
         | host:: _ ->
            (try [Hashtbl.find specific host ; default]
             with Not_found -> [default])
         | _       -> [default]
  in
  let rec kn = function
    | []           -> Response.fail_raise ~code:not_found "not found"
    | tree :: rest ->
       let path, l = get req.Request.meth req.Request.path_components tree in
       let rec fn = function
         | C(_,(freq,ph))::phs ->
            (try
               let handler = ph path in
               let (req,filter) = freq req in
               (req,filter,handler)
             with Pass -> fn phs)
         | [] -> kn rest
       in
       fn l
  in
  kn trees
