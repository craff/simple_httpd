
module S = Simple_httpd
module U = Simple_httpd_util

let now_ = Unix.gettimeofday

(* util: a little middleware collecting statistics *)
let middleware_stat () : S.Middleware.t * (unit -> string) =
  let n_req = ref 0 in
  let total_time_ = ref 0. in
  let parse_time_ = ref 0. in
  let build_time_ = ref 0. in
  let write_time_ = ref 0. in

  let m h req ~resp =
    incr n_req;
    let t1 = S.Request.start_time req in
    let t2 = now_ () in
    h req ~resp:(fun response ->
        let t3 = now_ () in
        resp response;
        let t4 = now_ () in
        total_time_ := !total_time_ +. (t4 -. t1);
        parse_time_ := !parse_time_ +. (t2 -. t1);
        build_time_ := !build_time_ +. (t3 -. t2);
        write_time_ := !write_time_ +. (t4 -. t3);
      )
  and get_stat () =
    Printf.sprintf "%d requests (average response time: %.3fms = %.3fms + %.3fms + %.3fms)"
      !n_req (!total_time_ /. float !n_req *. 1e3)
             (!parse_time_ /. float !n_req *. 1e3)
             (!build_time_ /. float !n_req *. 1e3)
             (!write_time_ /. float !n_req *. 1e3)
  in
  m, get_stat


let () =
  let addr = ref "127.0.0.1" in
  let port = ref 8080 in
  let j = ref 32 in
  Arg.parse (Arg.align [
      "--addr", Arg.Set_string addr, " set address";
      "-a", Arg.Set_string addr, " set address";
      "--port", Arg.Set_int port, " set port";
      "-p", Arg.Set_int port, " set port";
      "--debug", Arg.Int (fun n -> U.set_debug n), " set debug lvl";
      "-j", Arg.Set_int j, " maximum number of connections";
    ]) (fun _ -> raise (Arg.Bad "")) "echo [option]*";

  let listens = S.[{addr= !addr;port= !port;ssl=None}] in
  let server = S.create ~listens ~max_connections:!j () in
  Simple_httpd_camlzip.setup ~compress_above:1024 ~buf_size:(16*1024) server;

  let m_stats, get_stats = middleware_stat () in
  S.add_middleware server ~stage:(`Stage 1) m_stats;

  (* say hello *)
  S.add_route_handler ~meth:`GET server
    S.Route.(exact "hello" @/ string @/ return)
    (fun name _req -> S.Response.make_string ("hello " ^name ^"!\n"));

  (* compressed file access *)
  S.add_route_handler ~meth:`GET server
    S.Route.(exact "zcat" @/ string_urlencoded @/ return)
    (fun path _req ->
        let ic = open_in path in
        let str = S.Byte_stream.of_chan ic in
        let mime_type =
          try
            let p = Unix.open_process_in (Printf.sprintf "file -i -b %S" path) in
            try
              let s = ["Content-Type", String.trim (input_line p)] in
              ignore @@ Unix.close_process_in p;
              s
            with _ -> ignore @@ Unix.close_process_in p; []
          with _ -> []
        in
        S.Response.make_stream ~headers:mime_type str
      );

  (* echo request *)
  S.add_route_handler server
    S.Route.(exact "echo" @/ return)
    (fun req ->
        let q =
          S.Request.query req |> List.map (fun (k,v) -> Printf.sprintf "%S = %S" k v)
          |> String.concat ";"
        in
        S.Response.make_string
          (Format.asprintf "echo:@ %a@ (query: %s)@." S.Request.pp req q));

  (* file upload *)
  S.add_route_handler_stream ~meth:`PUT server
    S.Route.(exact "upload" @/ string @/ return)
    (fun path req ->
        U.debug (fun k->k "start upload %S, headers:\n%s\n\n%!" path
                     (Format.asprintf "%a" S.Headers.pp (S.Request.headers req)));
        try
          let oc = open_out @@ "/tmp/" ^ path in
          S.Byte_stream.to_chan oc req.S.Request.body;
          flush oc;
          S.Response.make_string "uploaded file"
        with e ->
          S.Response.fail ~code:500 "couldn't upload file: %s" (Printexc.to_string e)
      );

  (* stats *)
  S.add_route_handler server S.Route.(exact "stats" @/ return)
    (fun _req ->
       let stats = get_stats() in
       S.Response.make_string stats
    );

  (* VFS *)
  Simple_httpd_dir.add_vfs server
    ~config:(Simple_httpd_dir.config ~download:true
               ~dir_behavior:Simple_httpd_dir.Index_or_lists ())
    ~vfs:Vfs.vfs ~prefix:"vfs";

  (* main page *)
  S.add_route_handler server S.Route.(return)
    (fun _req ->
       let open Simple_httpd_html in
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
       let s = to_string_top h in
       S.Response.make_string ~headers:["content-type", "text/html"] s);

  List.iter S.(fun l ->
      Printf.printf "listening on http://%s:%d\n%!" l.addr l.port) (S.listens server);

  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
