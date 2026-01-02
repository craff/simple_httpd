(* echo.ml: a fairly complete example *)
open Simple_httpd
open Response_code
module H = Headers

(** Parse command line options *)

(** Default address, port, ssl configuration and folder for the site *)
let addr = ref "127.0.0.1"
let port = ref 8080
let top_dir = ref "/tmp" (* The folder where vfs_pack installed the file *)
let ssl_cert = ref ""
let ssl_priv = ref ""
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
      "--dir", Arg.Set_string top_dir, " set the top dir for file path";
      "--ssl", Tuple[Set_string ssl_cert; Set_string ssl_priv], " give ssl certificate and private key";
      "--ssl-ktls", Set ktls, " add support for kernel TLS";
    ] @ args)) (fun _ -> raise (Arg.Bad "")) "echo [option]*"

(** We configure ssl *)
let ssl =
  if !ssl_cert <> "" then
    Some Address.{ cert = !ssl_cert; priv = !ssl_priv;
                   protocol = Ssl.TLSv1_3; ktls = !ktls }
  else None

(** Server initialisation *)
let listens = [Address.make ~addr:!addr ~port:!port ?ssl ()]
let server = Server.create parameters ~listens

(** Compose the above filter with the compression filter
    provided by [Simple_httpd.Camlzip], than will compress output
    when [deflate] is accepted *)
let filter_zip =
  Camlzip.filter ~compress_above:1024 ~buf_size:(16*1024) ()

(** Compose with the stat filter that provided minimum statistics *)
let filter, get_stats =
  let filter_stat, get_stats = Stats.filter () in
  (Filter.compose_cross filter_zip filter_stat, get_stats)

(** Add a route answering 'Hello world' to [http://localhost/hello/world] *)
let _ =
  Server.add_route_handler ~meth:GET server ~filter
    Route.(exact "hello" @/ string @/ return)
    (fun name _req -> Response.make_string (Printf.sprintf "hello %s" name))

(** Add an echo request *)
let _ =
  Server.add_route_handler server ~filter
    Route.(exact "echo" @/ return)
    (fun req ->
      let q =
        Request.query req |> List.map (fun (k,v) -> Printf.sprintf "%S = %S" k v)
        |> String.concat ";"
      in
      Response.make_string
        (Format.asprintf "echo:@ %a@ (query: %s)@." Request.pp req q))

(** Add file upload *)
let _ =
  Server.add_route_handler_stream ~meth:PUT server ~filter
    Route.(exact "upload" @/ string @/ return)
    (fun path req ->
        Log.f (Req 0) (fun k->k "start upload %S, headers:\n%s\n\n%!" path
                     (Format.asprintf "%a" Headers.pp (Request.headers req)));
        try
          let oc = open_out @@ Filename.concat !top_dir path in
          Input.to_chan oc (Request.body req);
          flush oc;
          Response.make_string "uploaded file"
        with e ->
          Response.fail ~code:internal_server_error
            "couldn't upload file: %s" (Printexc.to_string e)
      )

(** Access to the statistics (the page can be tuned) *)
let _ =
  Server.add_route_handler_chaml server ~filter:filter_zip
    Route.(exact "stats" @/ return) get_stats

(** Access to the status of the server (the page can be tuned)  *)
let _ =
  Server.add_route_handler_chaml server
    ~filter:filter_zip Route.(exact "status" @/ return) (Status.html server)

(** Add a virtual file system VFS, produced by [simple-httpd-vfs-pack] from
    an actual folder *)
let _ =
  let vfs = Vfs.make ~top_dir:!top_dir () in
  Dir.add_vfs server
    ~config:(Dir.config ~download:true
               ~dir_behavior:Dir.Index_or_lists ())
    ~vfs:vfs
    ~prefix:"vfs" (* url is vfs/filename *)

(** Run a shell command (VERY UNSAFE!) *)
let _ =
  Server.add_route_handler ~meth:GET server ~filter
    Route.(exact "shell" @/ string @/ return)
    (fun cmd req ->
      let args = List.fold_left (fun acc (k,v) ->
                     let acc = if k <> "" then k::acc else acc in
                     let acc = if v <> "" then v::acc else acc in
                     acc) [cmd] (Request.query req)
      in
      let client = Request.client req in
      let args = Array.of_list (List.rev args) in
      let (_, io) = Process.create ~client cmd args in
      Response.make_stream (Input.of_io io);
    )

(** Add a directory, this is dynamic and changes are reflected immediately *)
let _ =
  Dir.add_dir_path server
    ~config:(Dir.config ~download:true
               ~dir_behavior:Dir.Index_or_lists ())
    ~prefix:"dir" ~dir:"../examples/files"

(** Add a route sending a compressed stream for the given file in the current
    directory, this example shows how to use Unix command within a request.
    Note that this example is unsafe, some stronger check should be performed on
    the filename *)
let _ =
  Server.add_route_handler ~meth:GET server ~filter
    Route.(exact "zcat" @/ string @/ return)
    (fun path _req ->
        if not (Filename.is_implicit path) then
	  Response.fail_raise ~code:Response_code.unauthorized "Unauthorized";
        let ic = open_in path in
        let str = Input.of_chan ic in
        let mime_type =
          try
            let p = Unix.open_process_in (Printf.sprintf "file -i -b %S" path) in
            try
              let s = [H.Content_Type, String.trim (input_line p)] in
              ignore @@ Unix.close_process_in p;
              s
            with _ -> ignore @@ Unix.close_process_in p; []
          with _ -> []
        in
        Response.make_stream ~headers:mime_type str
      )

(** Add an interactive terminal page using websocket! Must be protected by
    authentication and needs and html page to host it and xterm. *)
let _ =
  Server.add_route_handler ~meth:GET server
    Route.(exact "shell" @/ return) WebSocket.terminal_handler

(** Main page using the Html module (deprecated by vfs_pack and many other
    solutions, but still useful in some cases.*)
let _ =
  Server.add_route_handler_chaml server ~filter Route.return
    {chaml|
     <!DOCTYPE html>
     <html>
       <head>
         <title>index of echo</title>
       </head>
       <body>
	 <h3>welcome</h3>
	 <p><b>endpoints are</b></p>
	 <ul>
	   <li><pre>/ (GET)</pre> this file!</li>
           <li><pre>/hello/:name (GET)</pre> echo message</li>
           <li><pre><a href="/echo">echo</a></pre> echo back query</li>
           <li><pre>/upload/:path (PUT)</pre> to upload a file</li>
           <li><pre>/zcat/:path (GET)</pre> to download a file (deflate transfer-encoding)</li>
           <li><pre><a href="/stats/">/stats (GET)</a></pre> to access statistics</li>
           <li><pre><a href="/status/">/status (GET)</a></pre> to get server status</li>
           <li><pre><a href="/vfs/">/vfs (GET)</a></pre> to access a VFS
             embedded in the binary</li>
           <li><pre><a href="/vfs/shell.html">/vfs/shell.html (GET)</a></pre> to access a shell
             using web socket and xterm.js</li>
	 </ul>
       </body>
     </html>|chaml}

(** Start the server *)
let _ =
  Server.run server
