open Simple_httpd
module D = Dir
module Pf = Printf

let send_status status _req =
  Response.make_string (Async.string_status status ^ "\n")

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

let (args, parameters) = Server.args ()

module Params = (val parameters)

let _ =
  Arg.parse (Arg.align [
      "--addr", Set_string addr, " address to listen on";
      "-a", Set_string addr, " alias to --listen";
      "--port", Set_int port, " port to listen on";
      "-p", Set_int port, " alias to --port";
      "--ssl", Tuple[Set_string ssl_cert; Set_string ssl_priv], " give ssl certificate and private key";
      "--dir", Set_string dir, " directory to serve (default: \".\")";
      "--upload", Unit (fun () -> config.upload <- true), " enable file uploading";
      "--no-upload", Unit (fun () -> config.upload <- false), " disable file uploading";
      "--download", Unit (fun () -> config.download <- true), " enable file downloading";
      "--no-download", Unit (fun () -> config.download <- false), " disable file downloading";
      "--max-upload", String (fun i -> config.max_upload_size <- parse_size i),
                      " maximum size of files that can be uploaded";
      "--auto-index",
      Bool (fun b -> config.dir_behavior <-
               (if b then Index_or_lists else Lists)),
      " <bool> automatically redirect to index.html if present";
      "--delete", Unit (fun () -> config.delete <- true), " enable `delete` on files";
      "--no-delete", Unit (fun () -> config.delete <- false), " disable `delete` on files";
    ] @ args) (fun s -> dir := s) "http_of_dir [options] [dir]"


let ssl =
  if !ssl_cert <> "" then
    Some Address.{ cert = !ssl_cert; priv = !ssl_priv; protocol = Ssl.TLSv1_3 }
  else None

open Host

module Main = struct
  let addresses = [Address.make ~addr:!addr ~port:!port ?ssl ()]
  let hostnames = []

  module Init(I:Init) = struct
    open I
    let _ = add_dir_path ~config ~prefix:"" !dir
    let _ = add_route_handler Route.(exact "status" @/ return)
                 (send_status (Server.status server));
  end
end

let _ = start_server parameters [(module Main)]
