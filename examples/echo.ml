(* echo.ml: a fairly complete example *)
open Simple_httpd
open Response_code
module H = Headers

(** Parse command line options *)

(** Default address, port and maximum number of connections *)
let addr = ref "127.0.0.1"
let port = ref 8080
let top_dir = ref None

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
    ] @ args)) (fun _ -> raise (Arg.Bad "")) "echo [option]*"

(** Server initialisation *)
let listens = [Address.make ~addr:!addr ~port:!port ()]
let server = Server.create parameters ~listens

(** Compose the above filter with the compression filter
    provided by [Simple_httpd.Camlzip], than will compress output
    when [deflate] is accepted *)
let filter, get_stats =
  let filter_stat, get_stats = Stats.filter () in
  let filter_zip =
    Camlzip.filter ~compress_above:1024 ~buf_size:(16*1024) () in
  (Route.compose_cross filter_zip filter_stat, get_stats)

(** Add a route answering 'Hello world' to [http://localhost/hello/world] *)
let _ =
  Server.add_route_handler ~meth:GET server ~filter
    Route.(exact "hello" @/ string @/ return)
    (fun name _req -> Response.make_string ("hello " ^name ^"!\n"))

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
        Log.f (fun k->k "start upload %S, headers:\n%s\n\n%!" path
                     (Format.asprintf "%a" Headers.pp (Request.headers req)));
        try
          let oc = open_out @@ "/tmp/" ^ path in
          Input.to_chan oc (Request.body req);
          flush oc;
          Response.make_string "uploaded file"
        with e ->
          Response.fail ~code:internal_server_error
            "couldn't upload file: %s" (Printexc.to_string e)
      )

(** Access to the statistics *)
let _ =
  Server.add_route_handler server ~filter Route.(exact "stats" @/ return)
    (fun _req ->
       let stats = get_stats() in
       Response.make_string stats
    )

(** Add a virtual file system VFS, produced by [simple-httpd-vfs-pack] from
    an actual folger *)
let _ =
  let vfs = Vfs.make ?top_dir:!top_dir () in
  Dir.add_vfs server
    ~config:(Dir.config ~download:true
               ~dir_behavior:Dir.Index_or_lists ())
    ~vfs:vfs ~prefix:"vfs"

(** Add a route sending a compressed stream for the given file in the current
    directory *)
let _ =
  Server.add_route_handler ~meth:GET server ~filter
    Route.(exact "zcat" @/ string @/ return)
    (fun path _req ->
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

(** Main pagen using the Html module (deprecated by vfs_pack and many other
    solutions *)
let _ =
  Server.add_route_handler server ~filter Route.return
    (fun _req ->
       let open Html in
       let h = html [] [
           head[][title[][txt "index of echo"]];
           body[][
             h3[] [txt "welcome!"];
             p[] [b[] [txt "endpoints are:"]];
             ul[] [
               li[][pre[][txt "/hello/:name (GET)"]];
               li[][pre[][a[A.href "/echo/"][txt "echo"]; txt " echo back query"]];
               li[][pre[][txt "/upload/:path (PUT) to upload a file"]];
               li[][pre[][txt "/zcat/:path (GET) to download a file (deflate transfer-encoding)"]];
               li[][pre[][a[A.href "/stats/"][txt"/stats/"]; txt" (GET) to access statistics"]];
               li[][pre[][a[A.href "/vfs/"][txt"/vfs"]; txt" (GET) to access a VFS embedded in the binary"]];
             ]
           ]
         ] in
       let s = to_string ~top:true h in
       Response.make_string ~headers:[H.Content_Type, "text/html"] s)

(** Output a message before starting the server *)
let _ =
  Array.iter (fun l ->
    let open Address in
    Printf.printf "listening on http://%s:%d\n%!" l.addr l.port) (Server.listens server)

(** Start the server *)
let _ =
  Server.run server
