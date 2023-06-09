
(* serves some streams of events *)

open Simple_httpd
module H = Headers

let addr = ref "127.0.0.1"
let port = ref 8080
let t    = ref 1
let () =
Arg.parse (Arg.align [
      "-a", Arg.Set_string addr, " address to listen on";
      "-p", Arg.Set_int port, " port to listen on";
      "-r", Arg.Set_int t, " number of thread (1)";
      "--log", Arg.Int Log.set_log_lvl, " set log level";
      ]) (fun _ -> ()) "sse_clock [opt*]";

  let listens = [Address.make ~addr:!addr ~port:!port ()] in
  let server = Server.create ~num_thread:!t ~listens () in

  let extra_headers = [
    H.Access_Control_Allow_Origin, "*";
    H.Access_Control_Allow_Methods, "POST, GET, OPTIONS";
  ] in

  (* tick/tock goes the clock *)
  Server.add_route_server_sent_handler server Route.(exact "clock" @/ return)
    (fun _req (module EV : Server.SERVER_SENT_GENERATOR) ->
       Log.f (fun k->k "new connection");
       EV.set_headers extra_headers;
       let tick = ref true in
       while true do
         let now = Ptime_clock.now() in
         Log.f (fun k->k"send clock ev %s" (Format.asprintf "%a" Ptime.pp now));
         EV.send_event ~event:(if !tick then "tick" else "tock")
           ~data:(Ptime.to_rfc3339 now) ();
         tick := not !tick;
         Async.sleep 1.0;
       done;
    );

  (* just count *)
  Server.add_route_server_sent_handler server Route.(exact "count" @/ return)
    (fun _req (module EV : Server.SERVER_SENT_GENERATOR)  ->
       let n = ref 0 in
       while true do
         EV.send_event ~data:(string_of_int !n) ();
         incr n;
         Async.sleep 0.1;
       done;
    );
  Server.add_route_server_sent_handler server Route.(exact "count" @/ int @/ return)
    (fun n _req (module EV : Server.SERVER_SENT_GENERATOR)  ->
       for i=0 to n do
         EV.send_event ~data:(string_of_int i) ();
         Async.sleep 0.1;
       done;
       EV.close();
    );
  Server.add_route_handler server Route.(exact "fib" @/ int @/ return)
    (fun n _req ->
      let rec fib n =
        if n <= 1 then 1 else
          begin
	    if n > 10 then Async.yield ();
            fib (n-1) + fib (n-2)
          end
      in
      Response.make_string ("fib: " ^ string_of_int (fib n)^"\n")
    );

  Array.iter (fun l ->
    let open Address in
    Printf.printf "listening on http://%s:%d\n%!" l.addr l.port) (Server.listens server);

  Server.run server
