module S = Simple_httpd
module D = S.Dir
module Pf = Printf

let send_status status _req =
  let open Simple_httpd_domain in
  S.Response.make_string (string_status status ^ "\n")

let serve ~config ~timeout ~maxc (dir:string) listens t : _ result =
  let server = S.create ~timeout ~max_connections:maxc ~num_thread:t ~listens () in
  List.iter S.(fun l ->
      Printf.printf "serve directory %s on http://%s:%d\n%!"
        dir l.addr l.port) (S.listens server);

  D.add_dir_path ~config ~dir ~prefix:"" server;
  S.(add_route_handler server Route.(exact "status" @/ return)
       (send_status (S.status server)));
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
  let t        = ref (Domain.recommended_domain_count () - 1) in
  let log_folder = ref "" in
  let log_basename = ref "log" in
  let log_perm = ref 0o700 in
  let send_file_cache ~size:_ ~mime:_ ~accept_encoding:__ = D.SendFile in
  let mem_cache ~size:_ ~mime:_ ~accept_encoding:_ = D.MemCache in
  let deflate_cache ~size ~mime:_ ~accept_encoding =
    if (match size with None -> false | Some s -> s > 4_096)
       && List.mem "deflate" accept_encoding then
      D.CompressCache ("deflate",Simple_httpd_camlzip.deflate_string)
    else
      D.MemCache
  in
  Arg.parse (Arg.align [
      "--addr", Set_string addr, " address to listen on";
      "-a", Set_string addr, " alias to --listen";
      "--port", Set_int port, " port to listen on";
      "-p", Set_int port, " alias to --port";
      "--send-file", Unit (fun () -> config.cache <- send_file_cache), " use sendfile, not compatible with SSL but fast";
      "--cache", Unit (fun () -> config.cache <- mem_cache), " cache files in memory";
      "--cache-zlib", Unit (fun () -> config.cache <- deflate_cache), " cache compressed files in memory";
      "--ssl", Tuple[Set_string ssl_cert; Set_string ssl_priv], " give ssl certificate and private key";
      "--dir", Set_string dir_, " directory to serve (default: \".\")";
      "--log", Int S.set_log_lvl, " log level";
      "--log-folder", Set_string log_folder, " log folder";
      "--log-basename", Set_string log_basename, " log basename";
      "--log-perm", Set_int log_perm, " log permission";
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
  let listens = S.[{addr = !addr;port = !port;ssl; reuse = false}] in
  let timeout = !timeout in
  let maxc = !maxc in
  let _ = if !log_folder <> "" then
            S.set_log_folder ~basename:!log_basename ~perm:!log_perm !log_folder (!t + 1)
  in

  match serve ~config ~timeout ~maxc !dir_ listens !t with
  | Ok () -> ()
  | Error e ->
    raise e

let () = main ()
