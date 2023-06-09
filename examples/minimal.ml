
module S = Simple_httpd

let () =
  let addr = ref "127.0.0.1" in
  let port = ref 8080 in
  let j = ref 32 in
  let t = ref (Domain.recommended_domain_count ()) in
  Arg.parse (Arg.align [
      "--addr", Arg.Set_string addr, " set address";
      "--port", Arg.Set_int port, " set port";
      "--log", Arg.Int S.set_log_lvl, " set log level";
      "-j", Arg.Set_int j, " maximum number of connections";
      "-t", Arg.Set_int t, " number of threads/domains used";
    ]) (fun _ -> raise (Arg.Bad "")) "echo [option]*";

  let listens = [Address.make ~addr:!addr ~port:!port ()] in
  let server = S.create ~num_thread:!t ~listens ~max_connections:!j () in

  (* echo request *)
  S.add_route_handler server
    S.Route.(exact "echo" @/ return)
    (fun req ->
        S.Response.make_string
          (Format.asprintf "echo:@ %a@\n@." S.Request.pp req));

  Array.iter (fun l ->
    let open Address in
    Printf.printf "listening on http://%s:%d\n%!" l.addr l.port) (S.listens server);

  S.run server
