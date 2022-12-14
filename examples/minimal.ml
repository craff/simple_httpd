
module S = Tiny_httpd

let () =
  let port_ = ref 8080 in
  let j = ref 32 in
  let t = ref (Domain.recommended_domain_count ())
  Arg.parse (Arg.align [
      "--port", Arg.Set_int port_, " set port";
      "-p", Arg.Set_int port_, " set port";
      "--debug", Arg.Unit (fun () -> S._enable_debug true), " enable debug";
      "-j", Arg.Set_int j, " maximum number of connections";
      "-t", Arg.Set_int t, " number of threads/domains used";
    ]) (fun _ -> raise (Arg.Bad "")) "echo [option]*";

  let server = S.create ~num_thread:!t ~port:!port_ ~max_connections:!j () in

  (* echo request *)
  S.add_route_handler server
    S.Route.(exact "echo" @/ return)
    (fun req ->
        let q =
          S.Request.query req |> List.map (fun (k,v) -> Printf.sprintf "%S = %S" k v)
          |> String.concat ";"
        in
        S.Response.make_string
          (Ok (Format.asprintf "echo:@ %a@ (query: %s)@." S.Request.pp req q)));

  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
