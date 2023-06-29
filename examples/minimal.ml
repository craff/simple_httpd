open Simple_httpd

let () =
  let addr = ref "127.0.0.1" in
  let port = ref 8080 in
  let args, parameters = Server.args () in
  Arg.parse (Arg.align [
      "--addr", Arg.Set_string addr, " set address";
      "--port", Arg.Set_int port, " set port";
    ] @ args) (fun _ -> raise (Arg.Bad "")) "echo [option]*";

  let listens = [Address.make ~addr:!addr ~port:!port ()] in
  let server = Server.create parameters ~listens in

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
