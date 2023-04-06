
type buf = Buffer.t
type byte_stream = Simple_httpd_input.t

module U   = Simple_httpd_util
module D   = Simple_httpd_domain

module Out = Simple_httpd_output

module Input = Simple_httpd_input

let log     = D.log
let set_log_lvl = D.set_log_lvl
let set_log_folder = D.set_log_folder

exception Bad_req of int * string
let bad_reqf c fmt = Printf.ksprintf (fun s ->raise (Bad_req (c,s))) fmt

module Response_code = struct
  type t = int

  let ok = 200
  let not_found = 404
  let descr = function
    | 100 -> "Continue"
    | 200 -> "OK"
    | 201 -> "Created"
    | 202 -> "Accepted"
    | 204 -> "No content"
    | 300 -> "Multiple choices"
    | 301 -> "Moved permanently"
    | 302 -> "Found"
    | 304 -> "Not Modified"
    | 400 -> "Bad request"
    | 403 -> "Forbidden"
    | 404 -> "Not found"
    | 405 -> "Method not allowed"
    | 408 -> "Request timeout"
    | 409 -> "Conflict"
    | 410 -> "Gone"
    | 411 -> "Length required"
    | 413 -> "Payload too large"
    | 417 -> "Expectation failed"
    | 500 -> "Internal server error"
    | 501 -> "Not implemented"
    | 503 -> "Service unavailable"
    | n -> "Unknown response code " ^ string_of_int n (* TODO *)
end

module Meth = struct
  type t =
    | GET
    | PUT
    | POST
    | HEAD
    | DELETE

  let to_string = function
    | GET -> "GET"
    | PUT -> "PUT"
    | HEAD -> "HEAD"
    | POST -> "POST"
    | DELETE -> "DELETE"
  let pp out s = Format.pp_print_string out (to_string s)

  let of_string = function
    | "GET" -> GET
    | "PUT" -> PUT
    | "POST" -> POST
    | "HEAD" -> HEAD
    | "DELETE" -> DELETE
    | s -> bad_reqf 400 "unknown method %S" s
end

module Cookies = struct
  open Http_cookie
  exception BadCookies of string
  type nonrec t = (string * t) list

  let empty = []

  let parse s =
    match of_cookie s with
    | Ok l ->
       List.fold_left (fun acc c -> (name c, c)::acc) [] l
    | Error err -> raise (BadCookies err)

  let add name c cookies =
    let cookies = List.filter (fun (n, _) -> n <> name) cookies in
    (name, c) :: cookies

  let create : ?path:string ->
      ?domain:string ->
      ?expires:date_time ->
      ?max_age:int64 ->
      ?secure:bool ->
      ?http_only:bool ->
      ?same_site:same_site ->
      ?extension:string ->
      name:string ->
      string -> t -> t =
    fun ?path ?domain ?expires ?max_age ?secure ?http_only
        ?same_site ?extension ~name value cookies ->
      match create ?path ?domain ?expires ?max_age ?secure ?http_only
              ?same_site ?extension ~name value
      with Ok c -> add name c cookies
         | Error err -> raise (BadCookies err)

  let get name cookies =
    List.assoc name cookies

  let delete_all cookies =
    List.map (fun (name, c) -> (name, Http_cookie.expire c)) cookies

  let delete name cookies =
    try
      let c = List.assoc name cookies in
      let cookies = List.filter (fun (n,_) -> n <> name) cookies in
      (name, Http_cookie.expire c) :: cookies
    with
      Not_found -> cookies
end

module Headers = struct
  include Simple_httpd_header
  type t = (header * string) list
  let empty = []
  let contains name headers =
    List.exists (fun (n, _) -> eq name n) headers
  let get_exn ?(f=fun x->x) x h =
    snd (List.find (fun (x',_) -> eq x x') h) |> f
  let get ?(f=fun x -> x) x h =
    try Some (get_exn ~f x h) with Not_found -> None
  let remove x h =
    List.filter (fun (k,_) -> not (eq k x)) h
  let set x y h =
    (x,y) :: remove x h
  let pp out l =
    let pp_pair out (k,v) = Format.fprintf out "@[<h>%s: %s@]" (to_string k) v in
    Format.fprintf out "@[<v>%a@]" (Format.pp_print_list pp_pair) l
  let set_cookies cookies h =
    List.fold_left (fun h (_, c) ->
        (Set_Cookie, Http_cookie.to_set_cookie c) :: h) h cookies

  (*  token = 1*tchar
  tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." / "^" / "_"
           / "`" / "|" / "~" / DIGIT / ALPHA ; any VCHAR, except delimiters
  Reference: https://datatracker.ietf.org/doc/html/rfc7230#section-3.2 *)
  let parse_ ~buf (bs:byte_stream) : t * Cookies.t =
    let rec loop headers cookies =
      (try
        let k = parse bs in
        let v =
          try
            Input.read_line ~buf bs
          with _ -> bad_reqf 400 "invalid header value: %S" (to_string k)
        in
        let headers, cookies =
          if k = Cookie then
            begin
              let new_cookies = Cookies.parse v in
              (headers, List.fold_left (fun acc (name, c) ->
                            Cookies.add name c acc) cookies new_cookies)
            end
          else
            ((k,v)::headers, cookies)
        in
        fun () -> loop headers cookies
      with
      | End_of_headers ->
         assert (Input.read_char bs = '\n');
         (fun () -> (headers,cookies))
      | Invalid_header s ->
         let _ = Input.read_line ~buf bs in
         D.log ~lvl:1 (fun k -> k "ignoring unknown header starting with %s\n%!" s);
         (fun () -> loop headers cookies)) ()
    in
    loop [] []

end

module Request = struct
  type 'body t = {
    meth: Meth.t;
    host: string;
    client: D.client;
    headers: Headers.t;
    cookies: Cookies.t;
    http_version: int*int;
    path: string;
    path_components: string list;
    query: (string*string) list;
    body: 'body;
    start_time: float;
    trailer : (Headers.t * Cookies.t) option ref;
    (* updated after ready chunked body *)
  }

  let headers self = self.headers
  let cookies self = self.cookies
  let host self = self.host
  let meth self = self.meth
  let path self = self.path
  let body self = self.body
  let client self = self.client
  let start_time self = self.start_time
  let trailer self = !(self.trailer)

  let query self = self.query
  let get_header ?f self h = Headers.get ?f h self.headers
  let get_header_int self h = match get_header self h with
    | Some x -> (try Some (int_of_string x) with _ -> None)
    | None -> None
  let set_header k v self = {self with headers=Headers.set k v self.headers}
  let update_headers f self = {self with headers=f self.headers}
  let set_body b self = {self with body=b}

  let get_cookie self h =
    try Some (Cookies.get h self.cookies)
    with Not_found -> None

  (** Should we close the connection after this request? *)
  let close_after_req (self:_ t) : bool =
    match self.http_version with
    | 1, 1 -> get_header self Headers.Connection = Some"close"
    | 1, 0 -> not (get_header self Headers.Connection = Some"keep-alive")
    | _ -> false

  let pp_comp_ out comp =
    Format.fprintf out "[%s]"
      (String.concat ";" @@ List.map (Printf.sprintf "%S") comp)
  let pp_query out q =
    Format.fprintf out "[%s]"
      (String.concat ";" @@
       List.map (fun (a,b) -> Printf.sprintf "%S,%S" a b) q)
  let pp_ out self : unit =
    Format.fprintf out "{@[meth=%s;@ host=%s;@ headers=[@[%a@]];@ \
                        path=%S;@ body=?;@ path_components=%a;@ query=%a@]}"
      (Meth.to_string self.meth) self.host Headers.pp self.headers self.path
      pp_comp_ self.path_components pp_query self.query
  let pp out self : unit =
    Format.fprintf out "{@[meth=%s;@ host=%s;@ headers=[@[%a@]];@ path=%S;@ \
                        body=%S;@ path_components=%a;@ query=%a@]}"
      (Meth.to_string self.meth) self.host Headers.pp self.headers
      self.path self.body pp_comp_ self.path_components pp_query self.query

  (* decode a "chunked" stream into a normal stream *)
  let read_stream_chunked_ ~buf ~trailer (bs:byte_stream) : byte_stream =
    Input.read_chunked ~buf ~trailer
      ~fail:(fun s -> bad_reqf 400 "%s" s)
      bs

  let limit_body_size_ ~max_size (bs:byte_stream) : byte_stream =
    Input.limit_size_to ~max_size ~close_rec:false bs
      ~too_big:(fun size ->
          (* read too much *)
          bad_reqf 413
            "body size was supposed to be %d, but at least %d bytes received"
            max_size size
        )

  let limit_body_size ~max_size (req:byte_stream t) : byte_stream t =
    { req with body=limit_body_size_ ~max_size req.body }

  (* read exactly [size] bytes from the stream *)
  let read_exactly ~size (bs:byte_stream) : byte_stream =
    Input.read_exactly bs ~close_rec:false
      ~size ~too_short:(fun size ->
        bad_reqf 400 "body is too short by %d bytes" size)

  (* parse request, but not body (yet) *)
  let parse_req_start ~client ~buf (bs:byte_stream)
      : byte_stream t option =
    try
      D.log ~lvl:1 (fun k -> k "start reading request");
      D.register_starttime client;
      let line = Input.read_line ~buf bs in
      let start_time = Unix.gettimeofday () in
      let meth, path, version =
        try
          let meth, path, version = Scanf.sscanf line "%s %s HTTP/1.%d" (fun x y z->x,y,z) in
          if version != 0 && version != 1 then raise Exit;
          meth, path, version
        with e ->
          log ~lvl:1 (fun k->k "INVALID REQUEST LINE: `%s` (%s)" line (Printexc.to_string e));
          raise (bad_reqf 400 "Invalid request line")
      in
      let meth = Meth.of_string meth in
      log ~lvl:2 (fun k->k "got meth: %s, path %S" (Meth.to_string meth) path);
      let (headers, cookies) = Headers.parse_ ~buf bs in
      let host =
        match Headers.get Headers.Host headers with
        | None -> bad_reqf 400 "No 'Host' header in request"
        | Some h -> h
      in
      let path_components, query = Simple_httpd_util.split_query path in
      let path_components = Simple_httpd_util.split_on_slash path_components in
      let path_components = List.map U.percent_decode path_components in

      let query =
        match Simple_httpd_util.(parse_query query) with
        | Ok l -> l
        | Error e -> bad_reqf 400 "invalid query: %s" e
      in
      let req = {
        meth; query; host; client; path; path_components;
        headers; cookies; http_version=(1, version); body=bs; start_time;
        trailer = ref None;
      } in
      Some req
    with
    | End_of_file -> None
    | Bad_req _ as e -> raise e
    | e -> bad_reqf 400 "exception: %s" (D.printexn e)

  (* parse body, given the headers.
     @param tr_stream a transformation of the input stream. *)
  let parse_body_ ~tr_stream ~buf (req:byte_stream t) : byte_stream t =
    try
      Buffer.clear buf;
      let size =
        match Headers.get_exn Headers.Content_Length req.headers
              |> int_of_string with
        | n -> n (* body of fixed size *)
        | exception Not_found -> 0
        | exception _ -> bad_reqf 400 "invalid Content-Length"
      in
      let trailer bs =
        req.trailer := Some (Headers.parse_ ~buf bs)
      in
      let body =
        match get_header ~f:String.trim req Headers.Transfer_Encoding with
        | None ->
           let body = read_exactly ~size @@ tr_stream req.body in
           body
        | Some "chunked" ->
          let bs =
            read_stream_chunked_ ~buf ~trailer @@ tr_stream req.body
             (* body sent by chunks, with a trailer *)
          in
          let body = if size>0 then limit_body_size_ ~max_size:size bs else bs in
          body
        | Some s -> bad_reqf 500 "cannot handle transfer encoding: %s" s
      in
      {req with body}
    with
    | End_of_file -> bad_reqf 400 "unexpected end of file"
    | Bad_req _ as e -> raise e
    | e -> bad_reqf 400 "exception: %s" (D.printexn e)

  let read_body_full ~buf (self:byte_stream t) : string t =
    try
      let body = Input.read_all ~buf self.body in
      { self with body }
    with
    | e -> bad_reqf 500 "failed to read body: %s" (D.printexn e)

  module Internal_ = struct
    let parse_req_start ~buf ~client bs =
      parse_req_start ~client ~buf bs

    let parse_body ~buf req : byte_stream t =
      parse_body_ ~tr_stream:(fun s->s) ~buf req
  end
end

(*$R
  let q = "GET hello HTTP/1.1\r\nHost: coucou\r\nContent-Length: 11\r\n\r\nsalutationsSOMEJUNK" in
  let str = Simple_httpd.Input.of_string q in
  let buf = Buffer.create 256 in
  let r = Request.Internal_.parse_req_start ~buf ~client:Simple_httpd_domain.fake_client
             str in
  match r with
  | None -> assert_failure "should parse"
  | Some req ->
    let module H = Simple_httpd_header in
    assert_equal (Some "coucou") (Headers.get Headers.Host req.Request.headers);
    assert_equal (Some "11") (Headers.get Headers.Content_Length req.Request.headers);
    assert_equal "hello" req.Request.path;
    let req = Request.Internal_.parse_body ~buf req
      |> Request.read_body_full ~buf in
    assert_equal ~printer:(fun s->s) "salutations" req.Request.body;
    ()
*)

module Response = struct
  type body = String of string
            | Stream of byte_stream
            | File of int * Unix.file_descr * bool (* sent via sendfile *)
            | Void
  type t = {
    code: Response_code.t;
    headers: Headers.t;
    body: body;
  }

  let set_body body self = {self with body}
  let set_headers headers self = {self with headers}
  let headers self = self.headers
  let update_headers f self = {self with headers=f self.headers}
  let set_header k v self = {self with headers = Headers.set k v self.headers}
  let set_code code self = {self with code}

  let make_raw ?(cookies=[]) ?(headers=[]) ~code body : t =
    (* add content length to response *)
    let headers = Headers.set_cookies cookies headers in
    { code; headers; body=String body; }

  let make_raw_stream ?(cookies=[]) ?(headers=[]) ~code body : t =
    (* do not add content length to response *)
    let headers = Headers.set Headers.Transfer_Encoding "chunked" headers in
    let headers = Headers.set_cookies cookies headers in
    { code; headers; body=Stream body; }

  let make_raw_file ?(cookies=[]) ?(headers=[]) ~code ~close n body : t =
    (* add content length to response *)
    let headers = Headers.set_cookies cookies headers in
    { code; headers; body=File(n,body,close); }

  let make_void ?(cookies=[]) ?(headers=[]) ~code () : t =
    let headers = Headers.set_cookies cookies headers in
    { code; headers; body=Void; }

  let make_string ?cookies ?headers body =
    make_raw ?cookies ?headers ~code:200 body

  let make_stream ?cookies ?headers body =
    make_raw_stream ?cookies ?headers ~code:200 body

  let make_file ?cookies ?headers ~close n body =
    make_raw_file ?cookies ?headers ~code:200 ~close n body

  let make ?cookies ?headers r : t = match r with
    | String body -> make_raw ?cookies ?headers ~code:200 body
    | Stream body -> make_raw_stream ?cookies ?headers ~code:200 body
    | File(n,body,close)-> make_raw_file ?cookies ?headers ~code:200 ~close n body
    | Void -> make_void ?cookies ?headers ~code:200 ()

  let fail ?cookies ?headers ~code fmt =
    Printf.ksprintf (fun msg -> make_raw ?cookies ?headers ~code msg) fmt
  let fail_raise ~code fmt =
    Printf.ksprintf (fun msg -> bad_reqf code "%s" msg) fmt

  let pp out self : unit =
    let pp_body out = function
      | String s -> Format.fprintf out "%S" s
      | Stream _ -> Format.pp_print_string out "<stream>"
      | File   _ -> Format.pp_print_string out "<file>"
      | Void -> ()
    in
    Format.fprintf out "{@[code=%d;@ headers=[@[%a@]];@ body=%a@]}"
      self.code Headers.pp self.headers pp_body self.body

  let output_ (oc:Out.t) (self:t) : unit =
    Out.add_string oc "HTTP/1.1 ";
    Out.add_decimal oc self.code;
    Out.add_char oc ' ';
    Out.add_string oc (Response_code.descr self.code);
    Out.add_char oc '\r';
    Out.add_char oc '\n';
    let body = self.body in
    let headers =
      match body with
      | String "" | Void -> self.headers
      | String s ->
         Headers.set Headers.Content_Length (string_of_int (String.length s))
           self.headers
      | File(n, _, _) ->
         Headers.set Headers.Content_Length (string_of_int n) self.headers
      | Stream _ -> Headers.set Headers.Transfer_Encoding "chunked" self.headers
    in

    let self = {self with headers; body} in
    log ~lvl:2 (fun k->k "output response: %s"
                           (Format.asprintf "%a" pp {self with body=String "<…>"}));
    List.iter (fun (k,v) ->
        Out.add_string oc (Headers.to_string k);
        Out.add_char oc ':';
        Out.add_char oc ' ';
        Out.add_string oc v;
        Out.add_char oc '\r';
        Out.add_char oc '\n') headers;
    Out.add_string oc "\r\n";
    begin match body with
    | String "" | Void -> ()
    | String s         -> Out.output_str oc s
    | File (n, fd, false) -> Out.sendfile oc n fd
    | File (n, fd, true) ->
       (try Out.sendfile oc n fd; Unix.close fd
        with e -> Unix.close fd; raise e)
    | Stream str ->
       (try
          Out.output_chunked oc str;
          Input.close str;
        with e -> Input.close str; raise e)
    end;
    Out.flush oc

end

exception Pass (* raised to test the next handler *)

module Route = struct
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
    ; head : 'a otree
    ; delete : 'a otree }

  let empty_tree () =
    { get = empty_otree ()
    ; put = empty_otree ()
    ; post = empty_otree ()
    ; head = empty_otree ()
    ; delete = empty_otree () }

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

  let insert : type a b c.Meth.t -> (a,b) t -> c tree -> ((a,b) t -> c) -> unit =
    fun m p t x ->
      let t =
        let open Meth in
        match m with
        | GET -> t.get
        | PUT -> t.put
        | POST -> t.post
        | HEAD -> t.head
        | DELETE -> t.delete
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

  let get : Meth.t -> string list -> 'c tree -> string list * 'c cell list =
    fun m p t ->
      let open Meth in
      let t = match m with
        | GET -> t.get
        | PUT -> t.put
        | POST -> t.post
        | HEAD -> t.head
        | DELETE -> t.delete
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
    | _, Fire -> raise Pass
    | path, Rest -> f path
    | [], Compose _ -> raise Pass
    | x::path, Compose (ty, route) ->
       match (ty,route) with
       | (Exact s,route) -> if x <> s then raise Pass else eval path route f
       | (Int,    route) -> (try let x = int_of_string x in eval path route (f x)
                             with _ -> raise Pass)
       | (String, route) -> eval path route (f x)
    end

end

type filter = byte_stream Request.t -> byte_stream Request.t
                                       * (Response.t -> Response.t)
type path_handler = filter *
                    (string list -> Out.t -> byte_stream Request.t -> resp:(Response.t->unit) -> unit)

module type SERVER_SENT_GENERATOR = sig
  val set_headers : Headers.t -> unit
  val send_event :
    ?event:string ->
    ?id:string ->
    ?retry:string ->
    data:string ->
    unit -> unit
  val close : unit -> unit
end
type server_sent_generator = (module SERVER_SENT_GENERATOR)

type listening = Simple_httpd_domain.listening =
  {
    addr : string;
    port : int;
    ssl  : Ssl.context option ;
  }
(** Type describing addresses we want to listen too, provided
    here to avoid module opening *)

type t = {
  listens : listening list;

  timeout: float;

  num_thread: int;

  max_connections: int;

  masksigpipe: bool;

  buf_size: int;

  status : D.status;

  handlers : path_handler Route.tree array;
}

let listens self = self.listens

let status self = self.status

let active_connections _self = failwith "unimplemented"

let decode_request : (byte_stream -> byte_stream) -> (Headers.t -> Headers.t) ->
                     filter =
  (* turn it into a middleware *)
  fun tb th req ->
    let open Request in
    (* see if [f] modifies the stream *)
    ({req with body = tb req.body; headers = th req.headers }, fun r -> r)

let encode_response : (Response.body -> Response.body) ->
                      (Headers.t -> Headers.t) ->  filter =
  fun tb th req ->
    (req, fun resp ->
      let open Response in
      { resp with body = tb resp.body; headers = th resp.headers })

let compose_embrace : filter -> filter -> filter =
  fun f1 f2 req ->
    let (req, f2) = f2 req in
    let (req, f1) = f1 req in
    (req, fun resp -> f1 (f2 resp))

let compose_cross : filter -> filter -> filter =
  fun f1 f2 req ->
    let (req, f2) = f2 req in
    let (req, f1) = f1 req in
    (req, fun resp -> f2 (f1 resp))

(* route the given handler.
   @param tr_req wraps the actual concrete function returned by the route
   and makes it into a handler. *)
let add_route_handler_
    ?(filter=(fun x -> (x, fun x -> x)))
    ?adresses ?meth ~tr_req self route f =
  let fn route =
    let ph path =
      let f = Route.eval path route f in
      fun oc req ~resp -> tr_req oc req ~resp f
    in
    (filter, ph)
  in
  let gn t =
    match meth with
    | Some m ->
       Route.insert m route t fn
    | None ->
       Route.insert GET route t fn;
       Route.insert HEAD route t fn;
       Route.insert POST route t fn
  in
  let find_index x l =
    let rec fn i = function
    | [] -> invalid_arg "add_route: the server is not listening to that adress"
    | y::l -> if x = y then i else fn (i+1) l
    in
    fn 0 l
  in
  match adresses with
  | None -> Array.iter gn self.handlers
  | Some l ->
     let l = List.map (fun c -> find_index c self.listens) l in
     List.iter (fun i -> gn self.handlers.(i)) l

let add_route_handler ?filter ?adresses ?meth
    self route f : unit =
  let tr_req _oc req ~resp f =
    resp (f (Request.read_body_full ~buf:(Request.client req).buf req))
  in
  add_route_handler_ ?filter ?adresses ?meth self route ~tr_req f

let add_route_handler_stream ?filter ?adresses ?meth self route f =
  let tr_req _oc req ~resp f = resp (f req) in
  add_route_handler_ ?filter ?adresses ?meth self route ~tr_req f

let[@inline] _opt_iter ~f o = match o with
  | None -> ()
  | Some x -> f x

let add_route_server_sent_handler ?filter self route f =
  let tr_req oc req ~resp f =
    let buf = (Request.client req).buf in
    let req = Request.read_body_full ~buf req in
    let headers = ref Headers.(empty |> set Headers.Content_Type "text/event-stream") in

    (* send response once *)
    let resp_sent = ref false in
    let send_response_idempotent_ () =
      if not !resp_sent then (
        resp_sent := true;
        (* send 200 response now *)
        let initial_resp = Response.make_void ~headers:!headers ~code:200 () in
        resp initial_resp;
      )
    in

    let send_event ?event ?id ?retry ~data () : unit =
      send_response_idempotent_();
      _opt_iter event ~f:(fun e -> Out.printf oc "event: %s\n" e);
      _opt_iter id ~f:(fun e -> Out.printf oc "id: %s\n" e);
      _opt_iter retry ~f:(fun e -> Out.printf oc "retry: %s\n" e);
      let l = String.split_on_char '\n' data in
      List.iter (fun s -> Out.printf oc "data: %s\n" s) l;
      Out.add_char oc '\n'; (* finish group *)
      Out.flush oc;
    in
    let module SSG = struct
      let set_headers h =
        if not !resp_sent then (
          headers := List.rev_append h !headers;
          send_response_idempotent_()
        )
      let send_event = send_event
      let close () = raise Exit
    end in
    try f req (module SSG : SERVER_SENT_GENERATOR);
    with Exit -> Out.close oc
  in
  add_route_handler_ self ?filter ~meth:GET route ~tr_req f

let create
    ?(masksigpipe=true)
    ?(max_connections=32)
    ?(num_thread=Domain.recommended_domain_count () - 1)
    ?(timeout=300.0)
    ?(buf_size=16 * 2048)
    ?(listens = D.[{addr = "127.0.0.1"; port=8080; ssl = None}])
    () : t =
  let max_connections = max 4 max_connections in
  if num_thread <= 0 || max_connections < num_thread then
    invalid_arg "bad number of threads or max connections";
  let status = D.{
      nb_connections = Array.init num_thread (fun _ -> Atomic.make 0)
    }
  in
  let handlers = Array.init (List.length listens) (fun _ ->
                     Route.empty_tree ())
  in
  let self = {
    listens; masksigpipe; buf_size;
    max_connections;
    handlers; timeout; num_thread;
    status
    }
  in
  self

let handle_client_ (self:t) (client:D.client) : unit =
  let buf = client.buf in
  let oc  = Out.create ~buf_size:self.buf_size client in
  let is = Input.of_client ~buf_size:self.buf_size client in
  let continue = ref true in
  while !continue do
    match Request.parse_req_start ~client ~buf is with
    | None ->
      continue := false (* client is done *)

    | Some req ->
      log ~lvl:2 (fun k->k "req: %s" (Format.asprintf "@[%a@]" Request.pp_ req));

      if Request.close_after_req req then continue := false;

      try
        (* is there a handler for this path? *)
        let h = self.handlers.(client.accept_by) in
        let path, l = Route.get req.Request.meth req.Request.path_components h in
        let ((req,filter), handler) =
          let rec fn = function
            | Route.C(_,(freq,ph))::phs ->
               (try
                  let handler = ph path in
                  let req = freq req in
                  (req,handler)
                with Pass -> fn phs)
            | [] ->
               bad_reqf 404 "not found"
          in
          fn l
        in
        (* handle expect/continue *)
        begin match Request.get_header ~f:String.trim req Headers.Expect with
          | Some "100-continue" ->
            log ~lvl:2 (fun k->k "send back: 100 CONTINUE");
            Response.output_ oc (Response.make_raw ~code:100 "");
          | Some s -> bad_reqf 417 "unknown expectation %s" s
          | None -> ()
        end;

        (* now actually read request's body into a stream *)
        let req =
          Request.parse_body_ ~tr_stream:(fun s->s) ~buf req
        in

        (* how to reply *)
        let resp r =
          let r = filter r in
          try
            if Headers.get Headers.Connection r.Response.headers = Some"close" then
              continue := false;
            Response.output_ oc r;
          with Sys_error _
             | Unix.Unix_error _ as e ->
                continue := false;
                log ~lvl:1 (fun k -> k "fail to output response (%s)"
                                           (D.printexn e))
        in
        (* call handler *)
        handler oc req ~resp;
        log ~lvl:1 (fun k -> k "response sent after %fms" (1e3 *. (Unix.gettimeofday () -. req.start_time)));
        if !continue then D.yield ()
      with
      | Sys_error _ | Unix.Unix_error _ | D.ClosedByHandler | D.TimeOut as e ->
         log ~lvl:1 (fun k -> k "broken connection (%s)"
                                    (D.printexn e));
         continue := false; (* connection broken somehow *)

      | Bad_req (c,s) when (300 <= c && c <= 303) || (307 <= c && c <= 308) ->
         log ~lvl:1 (fun k -> k "redirect request (%s)" s);
         let res = Response.make_raw ~code:c "" in
         let res = Response.set_header Headers.Location s res in
         begin
           try Response.output_ oc res
           with Sys_error _ | Unix.Unix_error _ -> ()
         end;
         D.yield ()

      | Bad_req (c,s) ->
         (* connection error, close *)
         log ~lvl:1 (fun k -> k "error handling request (%s)" s);
         let res = Response.make_raw ~code:c s in
         begin
           try Response.output_ oc res
         with Sys_error _ | Unix.Unix_error _ -> ()
         end;
         if not (c < 500) then continue := false else D.yield ()

      | e ->
         log ~lvl:1 (fun k -> k "server error (%s)"
                                    (D.printexn e));
         continue := false;
         Response.output_ oc @@
           Response.fail ~code:500 "server error: %s" (D.printexn e)
  done;
  log ~lvl:2 (fun k->k "done with client, exiting");
  ()

let run (self:t) : (unit,_) result =
  try
    let handler client_sock = handle_client_ self client_sock in
    let maxc = self.max_connections in
    let a = D.run ~nb_threads:self.num_thread ~listens:self.listens
              ~maxc ~timeout:self.timeout ~status:self.status
              handler
    in
    Array.iter (fun d -> Domain.join d) a;
    Ok ()
  with e ->
    log ~lvl:1 (fun k -> k "server exit error (%s)"
                               (D.printexn e));
    Error e
