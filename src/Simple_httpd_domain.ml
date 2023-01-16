open Effect
open Effect.Deep
open Domain

module U = Simple_httpd_util

type status = {
    nb_availables : int Atomic.t;
    nb_connections : int array
  }

let max_domain = 16384

let all_domain_schedule = Array.make max_domain 0.0

let schedule () =
  let id = Domain.self () in
  let time = all_domain_schedule.((id :> int)) in
  let now = Unix.gettimeofday () in
  now >= time

let set_schedule delta =
  let id = Domain.self () in
  let now = Unix.gettimeofday () in
  all_domain_schedule.((id :> int)) <- now +. delta

let string_status st =
  let b = Buffer.create 128 in
  Printf.bprintf b "free sockets: %d\nconnections per sockets: [%t]"
    (Atomic.get st.nb_availables)
    (fun b ->
      Array.iteri (fun i a -> Printf.bprintf b "%s%d"
                                (if i = 0 then "" else ", ")
                                a) st.nb_connections);
  Buffer.contents b

let print_status ch st =
  output_string ch (string_status st)

type session_data = ..
type session_data += NoData

module MutexTmp = struct
  type t = bool Atomic.t

  let create () = Atomic.make false

  let try_lock m =
    Atomic.compare_and_set m false true

  let unlock m =
    Atomic.set m false
end

type client = {
    mutable connected : bool;
    sock : Unix.file_descr;
    mutable ssl  : Ssl.socket option;
    status : status;
    domain_id : int;
    mutable session : session option
  }

and session =
  { addr : string
  ; key : string
  ; mutex : MutexTmp.t
  ; mutable clients : client list
  ; mutable data : session_data
  }

let fake_client =
    { sock = Unix.stdout;
      status = { nb_availables = Atomic.make 0;
                 nb_connections = [||] };
      domain_id = 0;
      ssl = None;
      connected = false;
      session = None;
    }

type _ Effect.t +=
   | Read  : { sock: Unix.file_descr; fn: (unit -> int) }
               -> int Effect.t
   | Write : { sock: Unix.file_descr; fn: (unit -> int) }
               -> int Effect.t
   | Yield : unit Effect.t
   | Sleep : float -> unit Effect.t
   | Close : Unix.file_descr -> unit Effect.t
   | Lock  : bool Atomic.t -> unit Effect.t

exception Closed

type action = Read|Write

type pending =
  { action:action
  ; fn : unit -> int
  ; cont : (int, unit) continuation
  ; mutable arrival_time : float (* time it started to be pending *)
  }

type socket_type = Io of Unix.file_descr | Client of client

type socket_info =
  { ty : socket_type
  ; cl : 'a. ?effect:bool -> exn -> 'a
  ; mutable pd : pending option
  ; mutable seen_time : float    (* last time is was returned by select *)
  }

let apply c f1 f2 =
  match c.ssl with None -> f1 c.sock
                 | Some s -> f2 s

let now = Unix.gettimeofday

let sleep : float -> unit = fun t ->
  U.debug ~lvl:5 (fun k -> k "sleep(1,%e)" t);
  let t = now () +. t in
  perform (Sleep t)

module Mutex = struct
  include MutexTmp


  let lock : t -> unit = fun lk ->
    U.debug ~lvl:5 (fun k -> k "lock(1)");
    if not (try_lock lk) || schedule () then perform (Lock lk)
end

let yield () =
  U.debug ~lvl:5 (fun k -> k "yield(1)");
  perform Yield

let close c ?(effect=true) exn =
  U.debug ~lvl:3 (fun k -> k "closing because exception: %s. connected: %b (%d)"
                             (Printexc.to_string exn) c.connected
                             c.status.nb_connections.(c.domain_id));
  if c.connected then
    begin
      c.status.nb_connections.(c.domain_id) <-
        c.status.nb_connections.(c.domain_id) - 1 ;
      U.debug ~lvl:3 (fun k -> k "closed because exception: %s. connected: %b (%d)"
                                 (Printexc.to_string exn) c.connected
                                 c.status.nb_connections.(c.domain_id));
      begin
        match c.session with
        | None -> ()
        | Some sess ->
           Mutex.lock sess.mutex;
           sess.clients <- List.filter (fun c' -> c != c') sess.clients;
           Mutex.unlock sess.mutex
      end;
      begin
        let fn s =
          (try Ssl.shutdown s with _ -> ());
          Unix.close (Ssl.file_descr_of_socket s)
        in
        try apply c Unix.close fn with _ -> ()
      end;
      c.connected <- false;
      if effect then perform (Close c.sock);
    end;
  raise exn

let rec fread c s o l =
  try
    let n = apply c Unix.read Ssl.read s o l in
    U.debug ~lvl:5 (fun k -> k "read(1) %d/%d" n l);
    if n = 0 then raise Closed; n
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_))
     | Ssl.(Read_error(Error_want_read)) ->
        U.debug ~lvl:5 (fun k -> k "schedule write %d" l);
        perform_read c s o l
     | Ssl.(Read_error(Error_want_write)) ->
        U.debug ~lvl:5 (fun k -> k "schedule read(want_write) %d" l);
        perform (Write {sock = c.sock; fn = (fun () -> fread c s o l) })
     | exn -> close c exn

and read c s o l =
  if schedule () then yield (); fread c s o l

and perform_read c s o l =
  perform (Read {sock = c.sock; fn = (fun () -> fread c s o l) })

let rec fwrite c s o l =
  try
    let n = apply c Unix.single_write Ssl.write s o l in
    U.debug ~lvl:5 (fun k -> k "write(1) %d/%d" n l);
    if n = 0 then raise Closed; n
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_))
     | Ssl.(Write_error(Error_want_write)) ->
        U.debug ~lvl:5 (fun k -> k "exn schedule write %d" l);
        perform_write c s o l
     | Ssl.(Write_error(Error_want_read)) ->
        U.debug ~lvl:5 (fun k -> k "exn schedule write(want_read) %d" l);
        perform (Read {sock = c.sock; fn = (fun () -> fwrite c s o l) })
     | exn -> close c exn

and write c s o l =
  if schedule () then yield (); fwrite c s o l

and perform_write c s o l =
  perform (Write {sock = c.sock; fn = (fun () -> fwrite c s o l) })

let schedule_read sock fn =
  perform (Read {sock; fn })

let schedule_write sock fn =
  perform (Write {sock; fn
    })

module Io = struct
  let close s ?(effect=true) exn =
    (try Unix.close s with _ -> ());
    if effect then perform (Close s); raise exn

  let rec fread sock s o l =
  try
    let n = Unix.read sock  s o l in
    U.debug ~lvl:5 (fun k -> k "read(1) %d/%d" n l);
    if n = 0 then raise Closed; n
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_))
     | Ssl.(Read_error(Error_want_read)) ->
        U.debug ~lvl:5 (fun k -> k "schedule read %d" l);
        schedule_read sock (fun () -> fread sock s o l)
     | Ssl.(Read_error(Error_want_write)) ->
        U.debug ~lvl:5 (fun k -> k "schedule read(want_write) %d" l);
        schedule_write sock (fun () -> fread sock s o l)
     | exn -> close sock exn

  and read sock s o l =
    if schedule () then yield (); fread sock s o l

  let rec fwrite sock s o l =
  try
    let n = Unix.single_write sock s o l in
    U.debug ~lvl:5 (fun k -> k "write(1) %d/%d" n l);
    if n = 0 then raise Closed; n
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_))
     | Ssl.(Write_error(Error_want_write)) ->
        U.debug ~lvl:5 (fun k -> k "schedule write %d" l);
        schedule_write sock (fun () -> fwrite sock s o l)
     | Ssl.(Write_error(Error_want_read)) ->
        U.debug ~lvl:5 (fun k -> k "schedule write(want_read) %d" l);
        schedule_read sock (fun () -> fwrite sock s o l)
     | exn -> close sock exn

  and write sock s o l =
    if schedule () then yield (); fwrite sock s o l
end

let is_ipv6 addr = String.contains addr ':'

let connect addr port maxc =
  ignore (Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigpipe] : _ list);
  let sock =
    Unix.socket
      (if is_ipv6 addr then Unix.PF_INET6 else Unix.PF_INET)
      Unix.SOCK_STREAM
      0
  in
  Unix.set_nonblock sock;
  Unix.setsockopt_optint sock Unix.SO_LINGER None;
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.setsockopt sock Unix.SO_REUSEPORT true;
  let inet_addr = Unix.inet_addr_of_string addr in
  Unix.bind sock (Unix.ADDR_INET (inet_addr, port));
  Unix.listen sock maxc;
  sock

type listenning = {
    addr : string;
    port : int;
    ssl  : Ssl.context option ;
  }

type pollResult =
  | Timeout
  | Accept of (Unix.file_descr * listenning)
  | Action of pending * socket_info
  | Yield of ((unit,unit) continuation * float)

exception TimeOut

let loop id st listens maxc delta timeout handler () =
  let poll_list = Polly.create () in
  let listens =
    List.map (fun l ->
        let sock = connect l.addr l.port maxc in
        Polly.(add poll_list sock Events.(inp lor oneshot lor et));
        (sock, l)) listens
  in

  (* managment of max connection *)
  let accepting = ref true in
  let stop_accept () =
    accepting := false;
    List.iter (fun (sock, _) ->
        try Polly.(upd poll_list sock Events.empty)
        with e -> U.debug ~lvl:1 (fun k -> k "exn in stop accept: %s"
                                          (Printexc.to_string e));
               raise e) listens
  in
  let do_accept () =
    accepting := true;
    List.iter (fun (sock, _) ->
        try Polly.(upd poll_list sock Events.(inp lor oneshot lor et))
        with e -> U.debug ~lvl:1 (fun k -> k "exn in stop accept: %s"
                                          (Printexc.to_string e));
               raise e) listens
  in

  (* table of all sockets *)
  let pendings : (Unix.file_descr, socket_info) Hashtbl.t
    = Hashtbl.create 32
  in

  (* Queue for ready sockets *)
  let ready = Queue.create () in

  (* Managment of sleep *)
  let sleeps = ref [] in
  (* O(N) when N is the current number of sleep. Could use Map ?*)
  let add_sleep t cont =
    let rec fn acc = function
      | [] -> List.rev_append acc [(t,cont)]
      | (t',_)::_ as l when t < t' -> List.rev_append acc ((t,cont)::l)
      | c::l -> fn (c::acc) l
    in
    sleeps := fn [] !sleeps
  in
  (* amortized O(1) *)
  let get_sleep now =
    let rec fn l =
      match l with
      | (t,cont)::l when t <= now -> Queue.add (Yield(cont,now)) ready; fn l
      | l -> l
    in
    sleeps := fn !sleeps
  in

  (* Managment of lock *)
  let locks = U.LinkedList.create () in
  (* O(1) *)
  let add_lock lk cont = U.LinkedList.add_last (lk, cont) locks in
  (* O(N) when N is the number of waiting lock *)
  let get_lock now =
    let fn (lk, _) = Mutex.try_lock lk in
    let gn (_, cont) = Queue.add (Yield(cont,now)) ready in
    U.LinkedList.search_and_remove fn gn locks
  in
  let find now s =
    try Hashtbl.find pendings s with _ ->
      let i = { ty = Io s ; pd = None;
                seen_time = now;
                cl = Io.close s;
              } in
      Hashtbl.add pendings s i;
      (try Polly.(add poll_list s Events.(out lor inp lor hup lor err lor et))
       with exn -> Hashtbl.remove pendings s;
                   try Unix.close s with _ -> raise exn);
      i
  in

  (* managment of timeout and "bad" sockets *)
  (* O(N) but not run every "timeout" *)
  let next_timeout_check =
    ref (if timeout > 0.0 then Unix.gettimeofday () +. timeout
         else infinity) in
  let check now =
    U.debug ~lvl:3 (fun k -> k "CHECK");
    Hashtbl.filter_map_inplace (fun s c ->
        try
          let closing = (timeout > 0.0 && now -. c.seen_time > timeout) in
          U.debug ~lvl:3 (fun k -> k "closing if %f - %f  > %f => %b"
                                     now c.seen_time timeout closing);
          if closing then raise TimeOut;
          ignore (Unix.fstat s);
          Some c
        with e -> (try Polly.del poll_list s with _ -> ());
                  (try c.cl ~effect:false e with _ -> ());
                  None) pendings;
    if timeout > 0.0 then next_timeout_check := now +. timeout;
  in

  let rec poll () =
    U.debug (fun k -> k "poll %d %a (%d)" id print_status st (Hashtbl.length pendings));
    (* FIXME/CHECK *)
    let do_decr =
      if Hashtbl.length pendings = 0 then (Atomic.incr st.nb_availables; true)
      else false
    in
    if do_decr then Atomic.decr st.nb_availables;
    let now = now () in
    try
      (* O(n) when n is the number of waiting lock *)
      get_lock now;
      if now >= !next_timeout_check then check now;
      get_sleep now;
      let select_timeout =
        match Queue.is_empty ready, !sleeps with
        | false, _ -> 0.0
        | _, (t,_)::_ -> min delta (t -. now)
        | _ -> delta
      in
      U.debug ~lvl:7 (fun k -> k "Select with timeout: %f" select_timeout);
      if st.nb_connections.(id) < maxc && not !accepting then do_accept ();
      if st.nb_connections.(id) >= maxc && !accepting then stop_accept ();
      let select_timeout = int_of_float (1e3 *. select_timeout +. 1.0) in
      let fn _ sock _ =
        match List.find_opt (fun (s,_) -> s == sock) listens with
        | Some l -> Queue.add (Accept l) ready
        | None ->
           match find now sock with
           | { pd = None ; _ } -> ()
           | { pd = Some a ; _ } as p ->
              Queue.add (Action(a,p)) ready;
              p.pd <- None;
      in
      let res = Polly.wait poll_list 1000 select_timeout fn in
      U.debug ~lvl:6 (fun k -> k "polly returns %d sockets\n%!" res);
      try Queue.take ready with Queue.Empty -> Timeout
    with
    | exn -> U.debug ~lvl:0 (fun k -> k "UNEXPECTED EXCEPTION: %s\n%!"
                                        (Printexc.to_string exn));
             check now; poll () (* FIXME: which exception *)
  in
  let step () =
    try
      match poll () with
      | Timeout ->
         Domain.cpu_relax ();
      | Accept (lsock, linfo) ->
         U.debug (fun k -> k "accept connection from %d" id);
         Polly.(upd poll_list lsock Events.(inp lor oneshot lor et));
         let sock, _ = Unix.accept lsock in
         set_schedule delta;
         let client = { sock; status = st; ssl = None;
                        domain_id=id; connected = true; session = None } in
         let info = { ty = Client client
                    ; cl = close client
                    ; seen_time = now ()
                    ; pd = None
                    }
         in
         Hashtbl.add pendings sock info;
         begin
           try
             st.nb_connections.(id) <- st.nb_connections.(id) + 1;
             Polly.(add poll_list sock Events.(inp lor out lor hup lor err lor et));
             Unix.set_nonblock sock;
             begin
               match linfo.ssl with
               | Some ctx ->
                  let chan = Ssl.embed_socket sock ctx in
                  let rec fn () =
                    try Ssl.accept chan; 1
                    with
                    | Ssl.(Accept_error(Error_want_write)) ->
                       U.debug ~lvl:5 (fun k -> k "schedule accept(write)");
                       perform (Write {sock; fn })
                    | Ssl.(Accept_error(Error_want_read)) ->
                       U.debug ~lvl:5 (fun k -> k "schedule accept(read)");
                       perform (Read {sock; fn })
                  in
                  ignore (fn ());
                  client.ssl <- Some chan
               | None -> ()
             end;
             handler client
           with e -> (* error in set_nonblock or embed*)
             Hashtbl.remove pendings sock;
             Polly.(del poll_list sock);
             close ~effect:false client e
         end
      | Action ({ action; fn; cont; _ }, ({ cl; _ } as p)) ->
         p.pd <- None;
         let n = fn () in
         U.debug ~lvl:5 (fun k -> k "%s(2) %d"
                                    (if action = Read then "read" else "write") n);
         if n = 0 then cl Closed;
         set_schedule delta;
         continue cont n;
      | Yield(cont,_) ->
         U.debug ~lvl:5 (fun k -> k "yield(2)");
         set_schedule delta;
         continue cont ();
    with e ->
      U.debug (fun k -> k "exn: %s %a" (Printexc.to_string e) print_status st);
  in
  let step_handler () =
    try_with step ()
    { effc = (fun (type c) (eff: c Effect.t) ->
        match eff with
        | Yield ->
           Some (fun (cont : (c,_) continuation) ->
               Queue.add (Yield(cont, now ())) ready)
        | Sleep(t) ->
           Some (fun (cont : (c,_) continuation) ->
               add_sleep t cont)
        | Lock(lk) ->
           Some (fun (cont : (c,_) continuation) ->
               add_lock lk cont)
        | Read {sock; fn; _} ->
           Some (fun (cont : (c,_) continuation) ->
               let now = now () in
               let info = find now sock in
               assert (info.pd = None);
               info.pd <- Some { action=Read; fn; cont
                               ; arrival_time = now})
        | Write{sock; fn; _} ->
           Some (fun (cont : (c,_) continuation) ->
               let now = now () in
               let info = find now sock in
               assert (info.pd = None);
               info.pd <- Some { action=Write; fn; cont
                               ; arrival_time = now})
        | Close sock ->
           Some (fun (cont : (c,_) continuation) ->
               let info = find (now()) sock in
               (match info.ty with
                | Io s -> (try Unix.close s with _ -> ())
                | Client c -> assert(not c.connected));
               Hashtbl.remove pendings sock;
               (try Polly.del poll_list sock with _ -> ());
               continue cont ());
        | _ -> None
    )}
  in
  while true do
    step_handler ()
  done

let run ~nb_threads ~listens ~maxc ~delta ~timeout handler =
  let status = {
      nb_availables = Atomic.make 0;
      nb_connections = Array.init nb_threads (fun _ -> 0)
    }
  in
  let fn id = spawn (loop id status listens maxc delta timeout handler) in
  let r = Array.init (nb_threads - 1) fn in
  let _ = loop (nb_threads - 1) status listens maxc delta timeout handler () in
  r

let rec ssl_flush s =
  try ignore (Ssl.flush s); 1
  with Ssl.Flush_error(true) ->
    U.debug ~lvl:0 (fun k -> k "Retry in flush");
    schedule_write (Ssl.file_descr_of_socket s) (fun () -> ssl_flush s)

let flush c = apply c (fun _ -> ()) (fun s -> ignore (ssl_flush s))

(* All close above where because of error or socket closed on client side.
   close in Simple_httpd_server may be because there is no keep alive and
   the server close, so we flush before closing to handle the (very rare)
   ssl_flush exception above *)
let close c = flush c; close c Exit
