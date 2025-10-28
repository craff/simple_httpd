open Simple_httpd

let db_config = Caqti_connect_config.default
let uri = Uri.of_string "postgresql://caqti:caqti@localhost"

let raise_error res =
  match res with
  | Ok x -> x
  | Error err ->
     Log.(f (Req 0)) (fun k -> k "error in request: %a" Caqti_error.pp err);
     raise (Caqti_error.Exn err)

open Response_code
module H = Headers

(** Parse command line options *)

(** Default address, port and maximum number of connections *)
let addr = ref "127.0.0.1"
let port = ref 8080
let top_dir = ref None
let ssl_cert = ref ""
let ssl_priv = ref ""
let nb_keys = ref 1
let ktls = ref false

(** Server.args provides a bunch and standard option to control the
    maximum number of connections, logs, etc... *)
let args, parameters = Server.args ()

let _ =
  Arg.parse (Arg.align ([
      "--addr", Arg.Set_string addr, " set address";
      "-a", Arg.Set_string addr, " set address";
      "--port", Arg.Set_int port, " set port";
      "-p", Arg.Set_int port, " set port";
      "--dir", Arg.String (fun s -> top_dir := Some s), " set the top dir for file path";
      "--ssl", Tuple[Set_string ssl_cert; Set_string ssl_priv], " give ssl certificate and private key";
      "--ssl-ktls", Set ktls, " add support for kernel TLS";
      "--nb-keys", Arg.Set_int nb_keys, " number of data base key to use (default 1)";
    ] @ args)) (fun _ -> raise (Arg.Bad "")) "echo [option]*"

let ssl =
  if !ssl_cert <> "" then
    Some Address.{ cert = !ssl_cert; priv = !ssl_priv;
                   protocol = Ssl.TLSv1_3; ktls = !ktls }
  else None

(** Server initialisation *)
let listens = [Address.make ~addr:!addr ~port:!port ?ssl ()]
let server = Server.create parameters ~listens

let init () =
  Caqti_blocking.with_connection ~config:db_config uri (fun (module Db) ->
      let query =
        Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit)
        {sql|
         CREATE TABLE IF NOT EXISTS tests (key BIGINT PRIMARY KEY, count BIGINT DEFAULT 0)
         |sql}
  in
  let _ = raise_error @@ Db.exec query () in
  Ok ())

let incr req =
  Simple_httpd_caqti.with_session ~config:db_config uri req (fun (module Db) ->
      let key = Random.int !nb_keys in
      let query =
        Caqti_request.Infix.(Caqti_type.int ->! Caqti_type.int)
            {sql|
             INSERT INTO tests (count, key) VALUES (1, ?)
             ON CONFLICT (key)
             DO UPDATE
             SET count = tests.count + 1
             RETURNING count
             |sql}
      in
      let count = match Db.find query key with
        | Ok count -> count
        | _ -> 1
      in
      Ok (key, count))

let _ =
  Server.add_route_handler ~meth:GET server
    Route.(exact "incr" @/ return) (fun req ->
      let cookies, _ = Session.start_check ~create:true req in
      match incr req with
      | Ok (k, c) -> Response.make_string ~cookies
                       (Printf.sprintf "Key %d count is now %d" k c)
      | _    -> Response.fail ~code:internal_server_error "fail")

(** Start the server *)
let _ =
  let _ = init () in
  Server.run server
