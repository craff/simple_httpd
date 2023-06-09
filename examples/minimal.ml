open Simple_httpd

let () =
  let addr = ref "127.0.0.1" in
  let port = ref 8080 in
  let j = ref 32 in
  let t = ref (Domain.recommended_domain_count ()) in
  Arg.parse (Arg.align [
      "--addr", Arg.Set_string addr, " set address";
      "--port", Arg.Set_int port, " set port";
      "--log", Arg.Int Log.set_log_lvl, " set log level";
      "-j", Arg.Set_int j, " maximum number of connections";
      "-t", Arg.Set_int t, " number of threads/domains used";
    ]) (fun _ -> raise (Arg.Bad "")) "echo [option]*";

  let listens = [Address.make ~addr:!addr ~port:!port ()] in
  let server = Server.create ~num_thread:!t ~listens ~max_connections:!j () in

  (* echo request *)
  Server.add_route_handler server
    Route.(exact "echo" @/ return)
    (fun req ->
        Response.make_string
          (Format.asprintf "echo:@ %a@\n@." Request.pp req));

  Array.iter (fun l ->
      let open Address in
      Printf.printf "listening on http://%s:%d\n%!" l.addr l.port)
    (Server.listens server);

  Server.run server
