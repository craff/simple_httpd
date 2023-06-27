
open Simple_httpd
module H = Headers

let now = Unix.gettimeofday

(** default address, port and maximum number of connections *)
let addr = ref "127.0.0.1"
let port = ref 9080
let j = ref 32
let top_dir = ref ""
let timeout = ref (-1.0)

(** parse command line option *)
let _ =
  Arg.parse (Arg.align [
      "--addr", Arg.Set_string addr, " set address";
      "-a", Arg.Set_string addr, " set address";
      "--port", Arg.Set_int port, " set port";
      "-p", Arg.Set_int port, " set port";
      "--log", Arg.Int (fun n -> Log.set_log_lvl n), " set debug lvl";
      "--dir", Arg.Set_string top_dir, " set the top dir for file path";
      "--maxc", Arg.Set_int j, " maximum number of connections";
      "--timeout", Set_float timeout, " timeout in seconds, connection is closed after timeout second of inactivity (default: -1.0 means no timeout)";
    ]) (fun _ -> raise (Arg.Bad "")) "echo [option]*"

(** Server initialisation *)
let timeout = !timeout
let listens = [Address.make ~addr:!addr ~port:!port ()]
let server = Server.create ~timeout ~listens ~max_connections:!j ()

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
