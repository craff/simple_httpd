
module S = Simple_httpd

let () =
  let port_ = ref 8080 in
  let j = ref 32 in
  let t = ref (Domain.recommended_domain_count ()) in
  Arg.parse (Arg.align [
      "--port", Arg.Set_int port_, " set port";
      "--debug", Arg.Int S.set_debug, " set debug level";
      "-j", Arg.Set_int j, " maximum number of connections";
      "-t", Arg.Set_int t, " number of threads/domains used";
    ]) (fun _ -> raise (Arg.Bad "")) "echo [option]*";

  let server = S.create ~num_thread:!t ~port:!port_ ~max_connections:!j () in

  (* echo request *)
  S.add_route_handler server
    S.Route.(exact "echo" @/ return)
    (fun req ->
        S.Response.make_string
          (Ok (Format.asprintf "echo:@ %a@\n@." S.Request.pp req)));

  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
