
open Simple_httpd
module H = Headers

let now = Unix.gettimeofday

(** default address, port and maximum number of connections *)
let addr = ref "127.0.0.1"
let port = ref 9080
let top_dir = ref ""
let ssl_cert = ref ""
let ssl_priv = ref ""

(** parse command line option *)
let args, parameters = Server.args ()

let _ =
  Arg.parse (Arg.align ([
      "--addr", Arg.Set_string addr, " set address";
      "-a", Arg.Set_string addr, " set address";
      "--port", Arg.Set_int port, " set port";
      "-p", Arg.Set_int port, " set port";
      "--ssl", Tuple[Set_string ssl_cert; Set_string ssl_priv], " give ssl certificate and private key";
      "--dir", Arg.Set_string top_dir, " set the top dir for file path";
    ] @ args)) (fun _ -> raise (Arg.Bad "")) "echo [option]*"

let ssl =
  if !ssl_cert <> "" then
    begin
      Ssl_threads.init (); Ssl.init ();
      let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Server_context in
      Ssl.use_certificate ctx !ssl_cert !ssl_priv;
      Some ctx
    end
  else None

(** Server initialisation *)
let listens = [Address.make ~addr:!addr ~port:!port ?ssl ()]
let server = Server.create parameters ~listens

(** Add a virtual file system VFS, produced by [simple-httpd-vfs-pack] from
    an actual folger *)
let _ =
  let vfs = Vfs.make ~top_dir: !top_dir () in
  Dir.add_vfs server
    ~config:(Dir.config ~download:true
               ~dir_behavior:Dir.Index_or_lists ())
    ~vfs:vfs

let _ =
  Array.iter (fun l ->
    let open Address in
    Printf.printf "listening on http://%s:%d\n%!" l.addr l.port) (Server.listens server)

(** Start the server *)
let _ =
  Server.run server
