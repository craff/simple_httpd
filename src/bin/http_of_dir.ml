module S = Simple_httpd
module D = S.Dir
module Pf = Printf

let send_status status _req =
  let open Simple_httpd_domain in
  S.Response.make_string (string_status status ^ "\n")

let parse_size s : int =
  try Scanf.sscanf s "%dM" (fun n -> n * 1_024 * 1_024)
  with _ ->
  try Scanf.sscanf s "%dk" (fun n -> n * 1_024)
  with _ ->
  try int_of_string s
  with _ -> raise (Arg.Bad "invalid size (expected <int>[kM]?)")

let config =
  D.config ~dir_behavior:Index_or_lists ()

let dir      = ref "."
let addr     = ref "127.0.0.1"
let port     = ref 8080
let ssl_cert = ref ""
let ssl_priv = ref ""
let maxc     = ref 100
let delta    = ref 0.030
let timeout  = ref (-1.0)
let t        = ref (Domain.recommended_domain_count () - 1)
let log_folder = ref ""
let log_basename = ref "log"
let log_perm = ref 0o700

let send_file ~size:_ ~mime:_ ~accept_encoding:__ = D.SendFile
let mem_cache ~size:_ ~mime:_ ~accept_encoding:_ = D.MemCache
let deflate_cache ~size ~mime:_ ~accept_encoding =
  if (match size with None -> false | Some s -> s > 4_096)
     && List.mem "deflate" accept_encoding then
    D.CompressCache ("deflate",Simple_httpd_camlzip.deflate_string)
  else
    D.MemCache

let _ =
  Arg.parse (Arg.align [
      "--addr", Set_string addr, " address to listen on";
      "-a", Set_string addr, " alias to --listen";
      "--port", Set_int port, " port to listen on";
      "-p", Set_int port, " alias to --port";
      "--send-file", Unit (fun () -> config.cache <- send_file), " use sendfile, not compatible with SSL but fast";
      "--cache", Unit (fun () -> config.cache <- mem_cache), " cache files in memory";
      "--cache-zlib", Unit (fun () -> config.cache <- deflate_cache), " cache compressed files in memory";
      "--ssl", Tuple[Set_string ssl_cert; Set_string ssl_priv], " give ssl certificate and private key";
      "--dir", Set_string dir, " directory to serve (default: \".\")";
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
    ]) (fun s -> dir := s) "http_of_dir [options] [dir]"


let ssl =
  if !ssl_cert <> "" then
    begin
      Ssl_threads.init (); Ssl.init ();
      let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Server_context in
      Ssl.use_certificate ctx !ssl_cert !ssl_priv;
      Some ctx
    end
  else None

let _ =
  if !log_folder <> "" then
    S.set_log_folder ~basename:!log_basename ~perm:!log_perm !log_folder (!t + 1)

open Host

module Main = struct
  let addresses = [Address.make ~addr:!addr ~port:!port ?ssl ()]
  let hostnames = []

  module Init(I:HostInit) = struct
    open I
    let _ = add_dir_path ~config ~prefix:"" !dir
    let _ = add_route_handler S.Route.(exact "status" @/ return)
                 (send_status (S.status server));
  end
end

let timeout = !timeout
let max_connections = !maxc
let num_thread = !t

let _ = start_server ~timeout ~max_connections ~num_thread [(module Main)]
