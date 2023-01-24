
type buf = Buffer.t
type byte_stream = Simple_httpd_stream.t

module U   = Simple_httpd_util
module D   = Simple_httpd_domain

module Out = Simple_httpd_stream.Out_buf

module Byte_stream = Simple_httpd_stream

let debug     = U.debug
let set_debug = U.set_debug

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

type 'a resp_result = ('a, Response_code.t * string) result
let unwrap_resp_result = function
  | Ok x -> x
  | Error (c,s) -> raise (Bad_req (c,s))

module Meth = struct
  type t = [
    | `GET
    | `PUT
    | `POST
    | `HEAD
    | `DELETE
  ]

  let to_string = function
    | `GET -> "GET"
    | `PUT -> "PUT"
    | `HEAD -> "HEAD"
    | `POST -> "POST"
    | `DELETE -> "DELETE"
  let pp out s = Format.pp_print_string out (to_string s)

  let of_string = function
    | "GET" -> `GET
    | "PUT" -> `PUT
    | "POST" -> `POST
    | "HEAD" -> `HEAD
    | "DELETE" -> `DELETE
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
      let cookies = List.filter (fun (n, _) -> n <> name) cookies in
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
  type t = (string * string) list
  let empty = []
  let lower_eq s1 s2 =
    let len = String.length s1 in
    len = String.length s2 &&
      (try
        for i = 0 to len - 1 do
          if Char.lowercase_ascii s1.[i] <> Char.lowercase_ascii s2.[i] then
            raise Exit
        done;
        true
       with Exit -> false)
  let contains name headers =
    List.exists (fun (n, _) -> lower_eq name n) headers
  let get_exn ?(f=fun x->x) x h =
    snd (List.find (fun (x',_) -> lower_eq x x') h) |> f
  let get ?(f=fun x -> x) x h =
    try Some (get_exn ~f x h) with Not_found -> None
  let remove x h =
    List.filter (fun (k,_) -> not (lower_eq k x)) h
  let set x y h =
    (x,y) :: remove x h
  let pp out l =
    let pp_pair out (k,v) = Format.fprintf out "@[<h>%s: %s@]" k v in
    Format.fprintf out "@[<v>%a@]" (Format.pp_print_list pp_pair) l
  let set_cookies cookies h =
    List.fold_left (fun h (_, c) ->
        ("Set-Cookie", Http_cookie.to_set_cookie c) :: h) h cookies

  (*  token = 1*tchar
  tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." / "^" / "_"
           / "`" / "|" / "~" / DIGIT / ALPHA ; any VCHAR, except delimiters
  Reference: https://datatracker.ietf.org/doc/html/rfc7230#section-3.2 *)
  let is_tchar = function
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^'
    | '_' | '`'  | '|' | '~' -> true
    | _ -> false

  let parse_ ~buf (bs:byte_stream) : t * Cookies.t =
    let rec loop headers cookies =
      let k = Byte_stream.read_until ~buf  ':' bs in
      if k = "\r" then (
        (headers, cookies)
      ) else (
        let v =
          try
            if not (String.for_all is_tchar k) then (
              invalid_arg (Printf.sprintf "Invalid header key: %S" k));
            Byte_stream.read_line ~buf bs
          with _ -> bad_reqf 400 "invalid header key: %S" k
        in
        let headers, cookies =
          if lower_eq k "Cookie" then
            begin
              let new_cookies = Cookies.parse v in
              (headers, List.fold_left (fun acc (name, c) ->
                  Cookies.add name c acc) cookies new_cookies)
            end
          else
            ((k,v)::headers, cookies)
        in
        loop headers cookies
      )
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
  }

  let headers self = self.headers
  let cookies self = self.cookies
  let host self = self.host
  let meth self = self.meth
  let path self = self.path
  let body self = self.body
  let client self = self.client
  let start_time self = self.start_time

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
    | 1, 1 -> get_header self "connection" =Some"close"
    | 1, 0 -> not (get_header self "connection"=Some"keep-alive")
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
  let read_stream_chunked_ ~buf (bs:byte_stream) : byte_stream =
    Byte_stream.read_chunked ~buf
      ~fail:(fun s -> Bad_req (400, s))
      bs

  let limit_body_size_ ~max_size (bs:byte_stream) : byte_stream =
    Byte_stream.limit_size_to ~max_size ~close_rec:false bs
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
    Byte_stream.read_exactly bs ~close_rec:false
      ~size ~too_short:(fun size ->
        bad_reqf 400 "body is too short by %d bytes" size)

  (* parse request, but not body (yet) *)
  let parse_req_start ~client ~get_time_s ~buf (bs:byte_stream)
      : unit t option resp_result =
    try
      debug ~lvl:2 (fun k -> k "start reading request");
      D.register_starttime client;
      let line = Byte_stream.read_line ~buf bs in
      let start_time = get_time_s() in
      let meth, path, version =
        try
          let meth, path, version = Scanf.sscanf line "%s %s HTTP/1.%d" (fun x y z->x,y,z) in
          if version != 0 && version != 1 then raise Exit;
          meth, path, version
        with _ ->
          debug ~lvl:1 (fun k->k "INVALID REQUEST LINE: `%s`" line);
          raise (Bad_req (400, "Invalid request line"))
      in
      let meth = Meth.of_string meth in
      debug ~lvl:2 (fun k->k "got meth: %s, path %S" (Meth.to_string meth) path);
      let (headers, cookies) = Headers.parse_ ~buf bs in
      let host =
        match Headers.get "Host" headers with
        | None -> bad_reqf 400 "No 'Host' header in request"
        | Some h -> h
      in
      let path_components, query = Simple_httpd_util.split_query path in
      let path_components = Simple_httpd_util.split_on_slash path_components in
      let query =
        match Simple_httpd_util.(parse_query query) with
        | Ok l -> l
        | Error e -> bad_reqf 400 "invalid query: %s" e
      in
      let req = {
        meth; query; host; client; path; path_components;
        headers; cookies; http_version=(1, version); body=(); start_time;
      } in
      Ok (Some req)
    with
    | End_of_file -> Ok None
    | Bad_req (c,s) -> Error (c,s)
    | e -> Error (400, D.printexn e)

  (* parse body, given the headers.
     @param tr_stream a transformation of the input stream. *)
  let parse_body_ ~tr_stream ~buf (req:byte_stream t) : byte_stream t resp_result =
    try
      Buffer.clear buf;
      let size =
        match Headers.get_exn "Content-Length" req.headers |> int_of_string with
        | n -> n (* body of fixed size *)
        | exception Not_found -> 0
        | exception _ -> bad_reqf 400 "invalid Content-Length"
      in
      let body =
        match get_header ~f:String.trim req "Transfer-Encoding" with
        | None -> read_exactly ~size @@ tr_stream req.body
        | Some "chunked" ->
          let bs =
            read_stream_chunked_ ~buf @@ tr_stream req.body (* body sent by chunks *)
          in
          if size>0 then limit_body_size_ ~max_size:size bs else bs
        | Some s -> bad_reqf 500 "cannot handle transfer encoding: %s" s
      in
      Ok {req with body}
    with
    | End_of_file -> Error (400, "unexpected end of file")
    | Bad_req (c,s) -> Error (c,s)
    | e ->
      Error (400, D.printexn e)

  let read_body_full ~buf (self:byte_stream t) : string t =
    try
      let body = Byte_stream.read_all ~buf self.body in
      { self with body }
    with
    | Bad_req _ as e -> raise e
    | e -> bad_reqf 500 "failed to read body: %s" (D.printexn e)

  module Internal_ = struct
    let parse_req_start ~buf ~client ~get_time_s bs =
      parse_req_start ~client ~get_time_s ~buf bs |> unwrap_resp_result

    let parse_body ~buf req bs : _ t =
      parse_body_ ~tr_stream:(fun s->s) ~buf {req with body=bs} |> unwrap_resp_result
  end
end

(*$R
  let q = "GET hello HTTP/1.1\r\nHost: coucou\r\nContent-Length: 11\r\n\r\nsalutationsSOMEJUNK" in
  let str = Simple_httpd.Byte_stream.of_string q in
  let buf = Buffer.create 256 in
  let r = Request.Internal_.parse_req_start ~buf ~client:Simple_httpd_domain.fake_client
             ~get_time_s:(fun _ -> 0.) str in
  match r with
  | None -> assert_failure "should parse"
  | Some req ->
    assert_equal (Some "coucou") (Headers.get "Host" req.Request.headers);
    assert_equal (Some "coucou") (Headers.get "host" req.Request.headers);
    assert_equal (Some "11") (Headers.get "Content-Length" req.Request.headers);
    assert_equal "hello" req.Request.path;
    let req = Request.Internal_.parse_body ~buf req str
      |> Request.read_body_full ~buf in
    assert_equal ~printer:(fun s->s) "salutations" req.Request.body;
    ()
*)

module Response = struct
  type body = [`String of string | `Stream of byte_stream | `Void]
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
    { code; headers; body=`String body; }

  let make_raw_stream ?(cookies=[]) ?(headers=[]) ~code body : t =
    (* add content length to response *)
    let headers = Headers.set "Transfer-Encoding" "chunked" headers in
    let headers = Headers.set_cookies cookies headers in
    { code; headers; body=`Stream body; }

  let make_void ?(cookies=[]) ?(headers=[]) ~code () : t =
    let headers = Headers.set_cookies cookies headers in
    { code; headers; body=`Void; }

  let make_string ?cookies ?headers r = match r with
    | Ok body -> make_raw ?cookies ?headers ~code:200 body
    | Error (code,msg) -> make_raw ?cookies ?headers ~code msg

  let make_stream ?cookies ?headers r = match r with
    | Ok body -> make_raw_stream ?cookies ?headers ~code:200 body
    | Error (code,msg) -> make_raw ?cookies ?headers ~code msg

  let make ?cookies ?headers r : t = match r with
    | Ok (`String body) -> make_raw ?cookies ?headers ~code:200 body
    | Ok (`Stream body) -> make_raw_stream ?cookies ?headers ~code:200 body
    | Ok `Void -> make_void ?cookies ?headers ~code:200 ()
    | Error (code,msg) -> make_raw ?cookies ?headers ~code msg

  let fail ?cookies ?headers ~code fmt =
    Printf.ksprintf (fun msg -> make_raw ?cookies ?headers ~code msg) fmt
  let fail_raise ~code fmt =
    Printf.ksprintf (fun msg -> raise (Bad_req (code,msg))) fmt

  let pp out self : unit =
    let pp_body out = function
      | `String s -> Format.fprintf out "%S" s
      | `Stream _ -> Format.pp_print_string out "<stream>"
      | `Void -> ()
    in
    Format.fprintf out "{@[code=%d;@ headers=[@[%a@]];@ body=%a@]}"
      self.code Headers.pp self.headers pp_body self.body

  let output_ (oc:Out.t) (self:t) : unit =
    Out.printf oc "HTTP/1.1 %d %s\r\n" self.code
      (Response_code.descr self.code);
    let body = self.body in
    let headers =
      match body with
      | `String "" | `Void -> self.headers
      | _ -> Headers.set "Transfer-Encoding" "chunked" self.headers
    in

    let self = {self with headers; body} in
    debug ~lvl:2 (fun k->k "output response: %s"
                           (Format.asprintf "%a" pp {self with body=`String "<…>"}));
    List.iter (fun (k,v) ->
        Out.add_string oc k;
        Out.add_char oc ':';
        Out.add_char oc ' ';
        Out.add_string oc v;
        Out.add_char oc '\r';
        Out.add_char oc '\n') headers;
    Out.add_string oc "\r\n";
    begin match body with
    | `String "" | `Void -> ()
    | `String s -> Byte_stream.output_string_chunked oc s;
    | `Stream str ->
       try
         Byte_stream.output_chunked oc str;
         Byte_stream.close str;
       with e -> Byte_stream.close str; raise e
    end;
    Out.flush oc

end

module Route = struct
  type path = string list (* split on '/' *)

  type (_, _) comp =
    | Exact : string -> ('a, 'a) comp
    | Int : (int -> 'a, 'a) comp
    | String : (string -> 'a, 'a) comp
    | String_urlencoded : (string -> 'a, 'a) comp

  type (_, _) t =
    | Fire : ('b, 'b) t
    | Rest : {
        url_encoded: bool;
      } -> (string -> 'b, 'b) t
    | Compose: ('a, 'b) comp * ('b, 'c) t -> ('a, 'c) t

  let return = Fire
  let rest_of_path = Rest {url_encoded=false}
  let rest_of_path_urlencoded = Rest {url_encoded=true}
  let (@/) a b = Compose (a,b)
  let string = String
  let string_urlencoded = String_urlencoded
  let int = Int
  let exact (s:string) = Exact s
  let exact_path (s:string) tail =
    let rec fn = function
      | [] -> tail
      | ""::ls -> fn ls
      | s::ls -> exact s @/ fn ls
    in
    fn (String.split_on_char '/' s)
  let rec eval :
    type a b. path -> (a,b) t -> a -> b option =
    fun path route f ->
    begin match path, route with
      | [], Fire -> Some f
      | _, Fire -> None
      | _, Rest {url_encoded} ->
        let whole_path = String.concat "/" path in
        begin match
            if url_encoded
            then match Simple_httpd_util.percent_decode whole_path with
              | Some s -> s
              | None -> raise_notrace Exit
            else whole_path
          with
          | whole_path ->
            Some (f whole_path)
          | exception Exit -> None
        end
      | (c1 :: path'), Compose (comp, route') ->
        begin match comp with
          | Int ->
            begin match int_of_string c1 with
              | i -> eval path' route' (f i)
              | exception _ -> None
            end
          | String ->
            eval path' route' (f c1)
          | String_urlencoded ->
            begin match Simple_httpd_util.percent_decode c1 with
              | None -> None
              | Some s -> eval path' route' (f s)
            end
          | Exact s ->
            if s = c1 then eval path' route' f else None
        end
      | [], Compose (String, Fire) -> Some (f "") (* trailing *)
      | [], Compose (String_urlencoded, Fire) -> Some (f "") (* trailing *)
      | [], Compose _ -> None
    end

  let bpf = Printf.bprintf
  let rec pp_
    : type a b. Buffer.t -> (a,b) t -> unit
    = fun out -> function
      | Fire -> bpf out "/"
      | Rest {url_encoded} ->
        bpf out "<rest_of_url%s>" (if url_encoded then "_urlencoded" else "")
      | Compose (Exact s, tl) -> bpf out "%s/%a" s pp_ tl
      | Compose (Int, tl) -> bpf out "<int>/%a" pp_ tl
      | Compose (String, tl) -> bpf out "<str>/%a" pp_ tl
      | Compose (String_urlencoded, tl) -> bpf out "<enc_str>/%a" pp_ tl

  let to_string x =
    let b = Buffer.create 16 in
    pp_ b x;
    Buffer.contents b
  let pp out x = Format.pp_print_string out (to_string x)
end

module Middleware = struct
  type handler = byte_stream Request.t -> resp:(Response.t -> unit) -> unit
  type t = handler -> handler

  (** Apply a list of middlewares to [h] *)
  let apply_l (l:t list) (h:handler) : handler =
    List.fold_right (fun m h -> m h) l h

  let[@inline] nil : t = fun h -> h
end

(* a request handler. handles a single request. *)
type cb_path_handler = Out.t -> Middleware.handler

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

type t = {
  listens : D.listenning list;

  delta: float;

  timeout: float;

  num_thread: int;

  max_connections: int;

  masksigpipe: bool;

  buf_size: int;

  get_time_s : unit -> float;

  status : D.status;

  mutable handler: (string Request.t -> Response.t);
  (* toplevel handler, if any *)

  mutable middlewares : (int * Middleware.t) list;
  (** Global middlewares *)

  mutable middlewares_sorted : (int * Middleware.t) list lazy_t;
  (* sorted version of {!middlewares} *)

  mutable path_handlers : (unit Request.t -> cb_path_handler resp_result option) list;
  (* path handlers *)

}

let listens self = self.listens

let status self = self.status

let active_connections _self = failwith "unimplemented"

let add_middleware ~stage self m =
  let stage = match stage with
    | `Encoding -> 0
    | `Stage n when n < 1 -> invalid_arg "add_middleware: bad stage"
    | `Stage n -> n
  in
  self.middlewares <- (stage,m) :: self.middlewares;
  self.middlewares_sorted <- lazy (
    List.stable_sort (fun (s1,_) (s2,_) -> compare s1 s2) self.middlewares
  )

let add_decode_request_cb self f =
  (* turn it into a middleware *)
  let m h req ~resp =
    (* see if [f] modifies the stream *)
    let req0 = {req with Request.body=()} in
    match f req0 with
    | None -> h req ~resp (* pass through *)
    | Some (req1, tr_stream) ->
      let req = {req1 with Request.body=tr_stream req.Request.body} in
      h req ~resp
  in
  add_middleware self ~stage:`Encoding m

let add_encode_response_cb self f =
  let m h req ~resp =
    h req ~resp:(fun r ->
        let req0 = {req with Request.body=()} in
        (* now transform [r] if we want to *)
        match f req0 r with
        | None -> resp r
        | Some r' -> resp r')
  in
  add_middleware self ~stage:`Encoding m

let set_top_handler self f = self.handler <- f

type finaliser = Response.t -> Response.t
type 'a accept = 'a Request.t -> (finaliser, Response_code.t * string) result

(* route the given handler.
   @param tr_req wraps the actual concrete function returned by the route
   and makes it into a handler. *)
let add_route_handler_
    ?(accept=fun _req -> Ok (fun x -> x)) ?(middlewares=[])
    ?meth ~tr_req self (route:_ Route.t) f =
  let ph req : cb_path_handler resp_result option =
    match meth with
    | Some m when m <> req.Request.meth -> None (* ignore *)
    | _ ->
      begin match Route.eval req.Request.path_components route f with
        | Some handler ->
          (* we have a handler, do we accept the request based on its headers? *)
          begin match accept req with
            | Ok fn ->
              Some (Ok (fun oc ->
                  Middleware.apply_l middlewares @@
                    fun req ~resp -> let resp r = resp (fn r) in
                                     tr_req oc req ~resp handler))
            | Error _ as e -> Some e
          end
        | None ->
          None (* path didn't match *)
      end
  in
  self.path_handlers <- ph :: self.path_handlers

let add_route_handler (type a) ?accept ?middlewares ?meth
    self (route:(a,_) Route.t) (f:_) : unit =
  let tr_req _oc req ~resp f =
    resp (f (Request.read_body_full ~buf:(Request.client req).buf req))
  in
  add_route_handler_ ?accept ?middlewares ?meth self route ~tr_req f

let add_route_handler_stream ?accept ?middlewares ?meth self route f =
  let tr_req _oc req ~resp f = resp (f req) in
  add_route_handler_ ?accept ?middlewares ?meth self route ~tr_req f

let[@inline] _opt_iter ~f o = match o with
  | None -> ()
  | Some x -> f x

let add_route_server_sent_handler ?accept self route f =
  let tr_req oc req ~resp f =
    let buf = (Request.client req).buf in
    let req = Request.read_body_full ~buf req in
    let headers = ref Headers.(empty |> set "Content-Type" "text/event-stream") in

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
  add_route_handler_ self ?accept ~meth:`GET route ~tr_req f

let create
    ?(masksigpipe=true)
    ?(max_connections=32)
    ?(num_thread=Domain.recommended_domain_count () - 1)
    ?(delta=0.030)
    ?(timeout=300.0)
    ?(buf_size=16 * 2048)
    ?(get_time_s=Unix.gettimeofday)
    ?(listens = D.[{addr = "127.0.0.1"; port=8080; ssl = None}])
    ?(middlewares=[])
    () : t =
  let handler _req = Response.fail ~code:404 "no top handler" in
  let max_connections = max 4 max_connections in
  if num_thread <= 0 || max_connections < num_thread then
    invalid_arg "bad number of threads or max connections";
  let status = D.{
      nb_connections = Array.init num_thread (fun _ -> Atomic.make 0)
    }
  in
  let self = {
    listens; masksigpipe; handler; buf_size;
    max_connections; delta;
    path_handlers=[]; timeout; get_time_s; num_thread;
    middlewares=[]; middlewares_sorted=lazy [];
    status
    }
  in
  List.iter (fun (stage,m) -> add_middleware self ~stage m) middlewares;
  self

let find_map f l =
  let rec aux f = function
    | [] -> None
    | x::l' ->
      match f x with
        | Some _ as res -> res
        | None -> aux f l'
  in aux f l

let handle_client_ (self:t) (client:D.client) : unit =
  let buf = client.buf in
  let oc  = Out.create ~buf_size:self.buf_size client in
  let is = Byte_stream.of_client ~buf_size:self.buf_size client in
  let continue = ref true in
  while !continue do
    match Request.parse_req_start ~client ~get_time_s:self.get_time_s ~buf is with
    | Ok None ->
      continue := false (* client is done *)

    | Error (c,s) ->
      (* connection error, close *)
       U.debug ~lvl:1 (fun k -> k "error handling request (%s)" s);
       let res = Response.make_raw ~code:c s in
       begin
         try Response.output_ oc res
         with Sys_error _ | Unix.Unix_error _ -> ()
       end;
       continue := false

    | Ok (Some req) ->
      debug ~lvl:2 (fun k->k "req: %s" (Format.asprintf "@[%a@]" Request.pp_ req));

      if Request.close_after_req req then continue := false;

      try
        (* is there a handler for this path? *)
        let handler =
          match find_map (fun ph -> ph req) self.path_handlers with
          | Some f -> unwrap_resp_result f
          | None ->
            (fun _oc req ~resp ->
               let body_str = Request.read_body_full ~buf req in
               resp (self.handler body_str))
        in

        (* handle expect/continue *)
        begin match Request.get_header ~f:String.trim req "Expect" with
          | Some "100-continue" ->
            debug ~lvl:2 (fun k->k "send back: 100 CONTINUE");
            Response.output_ oc (Response.make_raw ~code:100 "");
          | Some s -> bad_reqf 417 "unknown expectation %s" s
          | None -> ()
        end;

        (* apply middlewares *)
        let handler =
          fun oc ->
            List.fold_right (fun (_, m) h -> m h)
              (Lazy.force self.middlewares_sorted) (handler oc)
        in

        (* now actually read request's body into a stream *)
        let req =
          Request.parse_body_ ~tr_stream:(fun s->s) ~buf {req with body=is}
          |> unwrap_resp_result
        in

        (* how to reply *)
        let resp r =
          try
            if Headers.get "Connection" r.Response.headers = Some"close" then
              continue := false;
            Response.output_ oc r;
          with Sys_error _
             | Unix.Unix_error _ as e ->
                continue := false;
                U.debug ~lvl:1 (fun k -> k "fail to output response (%s)"
                                           (D.printexn e))
        in
        (* call handler *)
        handler oc req ~resp;
        D.yield ()
      with
      | Sys_error _ | Unix.Unix_error _ | D.ClosedByHandler | D.TimeOut as e ->
         U.debug ~lvl:1 (fun k -> k "broken connection (%s)"
                                    (D.printexn e));
         continue := false; (* connection broken somehow *)
      | Bad_req (code,s) ->
         U.debug ~lvl:1 (fun k -> k "bad request (%s)" s);
         continue := false;
         Response.output_ oc @@ Response.make_raw ~code s
      | e ->
         U.debug ~lvl:1 (fun k -> k "server error (%s)"
                                    (D.printexn e));
         continue := false;
         Response.output_ oc @@
           Response.fail ~code:500 "server error: %s" (D.printexn e)
  done;
  debug ~lvl:2 (fun k->k "done with client, exiting");
  ()

let run (self:t) : (unit,_) result =
  try
    let handler client_sock = handle_client_ self client_sock in
    let maxc = self.max_connections in
    let a = D.run ~nb_threads:self.num_thread ~listens:self.listens
              ~maxc ~delta:self.delta ~timeout:self.timeout ~status:self.status
              handler
    in
    Array.iter (fun d -> Domain.join d) a;
    Ok ()
  with e ->
    U.debug ~lvl:1 (fun k -> k "server exit error (%s)"
                               (D.printexn e));
    Error e
