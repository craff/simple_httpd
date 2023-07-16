open Response_code

let fail_raise = Headers.fail_raise
let log = Log.f

type body = String of string
          | Stream of Input.t
          | File of
              { fd : Unix.file_descr
              ; size : int
              ; close : bool
                (** if using sendfile, one might want to maintain the fd open
                    for another request, sharing file descriptor would limit
                    the number of open files *)}
          | Void

type t = {
    code: Response_code.t;
    headers: Headers.t;
    body: body;
  }

let body self = self.body
let set_body body self = {self with body}
let set_headers headers self = {self with headers}
let headers self = self.headers
let update_headers f self = {self with headers=f self.headers}
let set_header k v self = {self with headers = Headers.set k v self.headers}
let set_code code self = {self with code}

let make_raw ?(cookies=[]) ?(headers=[]) ~code body : t =
  (* add content length to response *)
  let headers = Headers.set_cookies cookies headers in
  let body = if body = "" then Void else String body in
  { code; headers; body; }

let make_raw_stream ?(cookies=[]) ?(headers=[]) ~code body : t =
  (* do not add content length to response *)
  let headers = Headers.set Headers.Transfer_Encoding "chunked" headers in
  let headers = Headers.set_cookies cookies headers in
  { code; headers; body=Stream body; }

let make_raw_file ?(cookies=[]) ?(headers=[]) ~code ~close size body : t =
  (* add content length to response *)
  let headers = Headers.set_cookies cookies headers in
  { code; headers; body=File{size; fd=body; close}; }

let make_void ?(cookies=[]) ?(headers=[]) ~code () : t =
  let headers = Headers.set_cookies cookies headers in
  { code; headers; body=Void; }

let make_string ?cookies ?headers body =
  make_raw ?cookies ?headers ~code:ok body

let make_stream ?cookies ?headers body =
  make_raw_stream ?cookies ?headers ~code:ok body

let make_file ?cookies ?headers ~close n body =
  make_raw_file ?cookies ?headers ~code:ok ~close n body

let make ?cookies ?headers r : t = match r with
  | String body -> make_raw ?cookies ?headers ~code:ok body
  | Stream body -> make_raw_stream ?cookies ?headers ~code:ok body
  | File{size;fd=body;close}->
     make_raw_file ?cookies ?headers ~code:ok ~close size body
  | Void -> make_void ?cookies ?headers ~code:ok ()

let fail ?cookies ?headers ~code fmt =
  Printf.ksprintf (fun msg -> make_raw ?cookies ?headers ~code msg) fmt

let pp out self : unit =
  let pp_body out = function
    | String s -> Format.fprintf out "%S" s
    | Stream _ -> Format.pp_print_string out "<stream>"
    | File   _ -> Format.pp_print_string out "<file>"
    | Void -> ()
  in
  Format.fprintf out "{@[code=%d;@ headers=[@[%a@]];@ body=%a@]}"
    (self.code :> int) Headers.pp self.headers pp_body self.body

let output_ (oc:Output.t) (self:t) : unit =
  Output.add_string oc "HTTP/1.1 ";
  Output.add_decimal oc (self.code :> int);
  Output.add_char oc ' ';
  Output.add_string oc (Response_code.descr self.code);
  Output.add_char oc '\r';
  Output.add_char oc '\n';
  let body = self.body in
  let headers =
    match body with
    | String "" | Void -> self.headers
    | String s ->
       Headers.set Headers.Content_Length (string_of_int (String.length s))
         self.headers
    | File{size;_} ->
       Headers.set Headers.Content_Length (string_of_int size) self.headers
    | Stream _ -> Headers.set Headers.Transfer_Encoding "chunked" self.headers
  in

  let self = {self with headers; body} in
  log (Req 2) (fun k->k "output response: %s"
                       (Format.asprintf "%a" pp {self with body=String "<…>"}));
  List.iter (fun (k,v) ->
      Output.add_string oc (Headers.to_string k);
      Output.add_char oc ':';
      Output.add_char oc ' ';
      Output.add_string oc v;
      Output.add_char oc '\r';
      Output.add_char oc '\n') headers;
  Output.add_string oc "\r\n";
  begin match body with
  | String "" | Void -> ()
  | String s         -> Output.output_str oc s
  | File {size; fd; close=false} -> Output.sendfile oc size fd
  | File {size; fd; close=true} ->
     (try Output.sendfile oc size fd; Unix.close fd
      with e -> Unix.close fd; raise e)
  | Stream str ->
     (try
        Output.output_chunked oc str;
        Input.close str;
      with e -> Input.close str; raise e)
  end;
  Output.flush oc
