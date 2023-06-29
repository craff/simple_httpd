
open Simple_httpd
module H = Headers

let now = Unix.gettimeofday

(** default address, port and maximum number of connections *)
let addr = ref "127.0.0.1"
let port = ref 9080
let top_dir = ref ""

(** parse command line option *)
let args, parameters = Server.args ()
let _ =
  Arg.parse (Arg.align ([
      "--addr", Arg.Set_string addr, " set address";
      "-a", Arg.Set_string addr, " set address";
      "--port", Arg.Set_int port, " set port";
      "-p", Arg.Set_int port, " set port";
      "--dir", Arg.Set_string top_dir, " set the top dir for file path";
    ] @ args)) (fun _ -> raise (Arg.Bad "")) "echo [option]*"

(** Server initialisation *)
let listens = [Address.make ~addr:!addr ~port:!port ()]
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
