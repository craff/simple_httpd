
open Simple_httpd
module H = Headers

let now_ = Unix.gettimeofday

(* util: a little middleware collecting statistics *)
let filter_stat () : Route.filter * (unit -> string) =
  let n_req = ref 0 in
  let total_time_ = ref 0. in
  let parse_time_ = ref 0. in
  let build_time_ = ref 0. in

  let m req =
    incr n_req;
    let t1 = Request.start_time req in
    let t2 = now_ () in
    (req, fun response ->
        let t3 = now_ () in
        total_time_ := !total_time_ +. (t3 -. t1);
        parse_time_ := !parse_time_ +. (t2 -. t1);
        build_time_ := !build_time_ +. (t3 -. t2);
        response)
  and get_stat () =
    Printf.sprintf "%d requests (average response time: %.3fms = %.3fms + %.3fms)"
      !n_req (!total_time_ /. float !n_req *. 1e3)
             (!parse_time_ /. float !n_req *. 1e3)
             (!build_time_ /. float !n_req *. 1e3)
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
      "--log", Arg.Int (fun n -> Log.set_log_lvl n), " set debug lvl";
      "-j", Arg.Set_int j, " maximum number of connections";
    ]) (fun _ -> raise (Arg.Bad "")) "echo [option]*";

  let listens = [Address.make ~addr:!addr ~port:!port ()] in
  let server = Server.create ~listens ~max_connections:!j () in
  let filter_stat, get_stats = filter_stat () in
  let filter_zip =
    Simple_httpd_camlzip.filter ~compress_above:1024 ~buf_size:(16*1024) ()
  in
  let filter = Route.compose_cross filter_zip filter_stat in

  (* say hello *)
  Server.add_route_handler ~meth:GET server ~filter
    Route.(exact "hello" @/ string @/ return)
    (fun name _req -> Response.make_string ("hello " ^name ^"!\n"));

  (* compressed file access *)
  Server.add_route_handler ~meth:GET server
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
      );

  (* echo request *)
  Server.add_route_handler server
    Route.(exact "echo" @/ return)
    (fun req ->
        let q =
          Request.query req |> List.map (fun (k,v) -> Printf.sprintf "%S = %S" k v)
          |> String.concat ";"
        in
        Response.make_string
          (Format.asprintf "echo:@ %a@ (query: %s)@." Request.pp req q));

  (* file upload *)
  Server.add_route_handler_stream ~meth:PUT server
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
          Response.fail ~code:500 "couldn't upload file: %s" (Printexc.to_string e)
      );

  (* stats *)
  Server.add_route_handler server Route.(exact "stats" @/ return)
    (fun _req ->
       let stats = get_stats() in
       Response.make_string stats
    );

  (* VFS *)
  Dir.add_vfs server
    ~config:(Dir.config ~download:true
               ~dir_behavior:Dir.Index_or_lists ())
    ~vfs:Vfs.vfs ~prefix:"vfs";

  (* main page *)
  Server.add_route_handler server Route.(return)
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
       Response.make_string ~headers:[H.Content_Type, "text/html"] s);

  Array.iter (fun l ->
    let open Address in
    Printf.printf "listening on http://%s:%d\n%!" l.addr l.port) (Server.listens server);

  Server.run server
