module S = Simple_httpd
module U = Simple_httpd_util
module D = Simple_httpd_dir
module Pf = Printf

let serve ~config ~delta ~timeout ~maxc (dir:string) listens t : _ result =
  let server = S.create ~delta ~timeout ~max_connections:maxc ~num_thread:t ~listens () in
  List.iter S.(fun l ->
      Printf.printf "serve directory %s on http://%s:%d\n%!"
        dir l.addr l.port) (S.listens server);

  D.add_dir_path ~config ~dir ~prefix:"" server;
  S.run server

let parse_size s : int =
  try Scanf.sscanf s "%dM" (fun n -> n * 1_024 * 1_024)
  with _ ->
  try Scanf.sscanf s "%dk" (fun n -> n * 1_024)
  with _ ->
  try int_of_string s
  with _ -> raise (Arg.Bad "invalid size (expected <int>[kM]?)")

let main () =
  let config =
    D.config ~dir_behavior:Index_or_lists ()
  in
  let dir_     = ref "." in
  let addr     = ref "127.0.0.1" in
  let port     = ref 8080 in
  let ssl_cert = ref "" in
  let ssl_priv = ref "" in
  let maxc     = ref 100 in
  let delta    = ref 0.030 in
  let timeout  = ref (-1.0) in
  let t        = ref (Domain.recommended_domain_count ()) in
  Arg.parse (Arg.align [
      "--addr", Set_string addr, " address to listen on";
      "-a", Set_string addr, " alias to --listen";
      "--port", Set_int port, " port to listen on";
      "-p", Set_int port, " alias to --port";
      "--cache", Unit (fun () -> config.cache <- SimpleCache), " cache files in memory";
      "--cache-zlib", Unit (fun () -> config.cache <- Simple_httpd_camlzip.(ZlibCache  { chk = accept_deflate; cmp = deflate_string})), " cache compressed files in memory";
      "--ssl", Tuple[Set_string ssl_cert; Set_string ssl_priv], " give ssl certificate and private key";
      "--dir", Set_string dir_, " directory to serve (default: \".\")";
      "--debug", Int U.set_debug, " debug mode";
      "--upload", Unit (fun () -> config.upload <- true), " enable file uploading";
      "--no-upload", Unit (fun () -> config.upload <- false), " disable file uploading";
      "--download", Unit (fun () -> config.download <- true), " enable file downloading";
      "--no-download", Unit (fun () -> config.download <- false), " disable file downloading";
      "--max-upload", String (fun i -> config.max_upload_size <- parse_size i),
                      " maximum size of files that can be uploaded";
      "--delta", Set_float delta, " schedule write/read/lock after delta seconds (default 30ms)";
      "--timeout", Set_float timeout, " timeout in seconds, connection is closed after timeout second of inactivity (default: -1.0 means no timeout)";
      "--auto-index",
      Bool (fun b -> config.dir_behavior <-
               (if b then Index_or_lists else Lists)),
      " <bool> automatically redirect to index.html if present";
      "--delete", Unit (fun () -> config.delete <- true), " enable `delete` on files";
      "--no-delete", Unit (fun () -> config.delete <- false), " disable `delete` on files";
      "--maxc", Set_int maxc, " maximum number of simultaneous connections (default 100)";
      "-t", Set_int t, " maximum number of threads";
    ]) (fun s -> dir_ := s) "http_of_dir [options] [dir]";

  let ssl =
    if !ssl_cert <> "" then
      begin
        Ssl_threads.init (); Ssl.init ();
        let ctx = Ssl.create_context Ssl.SSLv23 Ssl.Server_context in
        Ssl.use_certificate ctx !ssl_cert !ssl_priv;
        Some ctx
      end
    else None
  in
  let listens = S.[{addr = !addr;port = !port;ssl}] in
  let timeout = !timeout and delta = !delta in
  let maxc = !maxc in

  match serve ~config ~delta ~timeout ~maxc !dir_ listens !t with
  | Ok () -> ()
  | Error e ->
    raise e

let () = main ()
