open Simple_httpd

let db_config = Uri.of_string "postgresql://caqti:caqti@localhost"

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
      "--nb-keys", Arg.Set_int nb_keys, " number of data base key to use (default 1)";
    ] @ args)) (fun _ -> raise (Arg.Bad "")) "echo [option]*"

let ssl =
  if !ssl_cert <> "" then
    Some Address.{ cert = !ssl_cert; priv = !ssl_priv; protocol = Ssl.TLSv1_3 }
  else None

(** Server initialisation *)
let listens = [Address.make ~addr:!addr ~port:!port ?ssl ()]
let server = Server.create parameters ~listens

module Rapper_helper = Rapper.Make_helper (struct
  type +'a t = 'a

  let return x = x

  let map x = x

  module Stream = Simple_httpd_caqti.Stream
end)

let init () =
  Simple_httpd_caqti.with_connection db_config (fun db ->
  let query =
    [%rapper execute
        {sql|
         CREATE TABLE IF NOT EXISTS tests (key BIGINT PRIMARY KEY, count BIGINT DEFAULT 0)
         |sql}]
  in
  let _ = raise_error @@ query () db in
  Ok ())

let incr req =
  Simple_httpd_caqti.with_session ~db_config req (fun db ->
      let key = Random.int !nb_keys in
      let query =
        [%rapper get_opt
            {sql|
             INSERT INTO tests (count, key) VALUES (1, %int{key})
             ON CONFLICT (key)
             DO UPDATE
             SET count = tests.count + 1
             RETURNING @int{count}
             |sql}]
      in
      let count = match query ~key db with
        | Ok (Some count) -> count
        | _ -> 1
      in
      Ok (key, count))

let _ =
  Server.add_route_handler ~meth:GET server
    Route.(exact "incr" @/ return) (fun req ->
      let cookies, _ = Session.start_check ~create:true req in
      match incr req with
      | Ok (k, c) -> Response.make_string ~cookies (Printf.sprintf "%d: %d" k c)
      | _    -> Response.fail ~code:internal_server_error "fail")

(** Start the server *)
let _ =
  let _ = init () in
  Server.run server
