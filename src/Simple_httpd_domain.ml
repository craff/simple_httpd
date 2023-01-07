open Effect
open Effect.Deep
open Domain

module U = Simple_httpd_util

type status = {
    nb_availables : int Atomic.t;
    nb_connections : int Atomic.t array
  }

let string_status st =
  let b = Buffer.create 128 in
  Printf.bprintf b "free sockets: %d\nconnections per sockets: [%t]"
    (Atomic.get st.nb_availables)
    (fun b ->
      Array.iteri (fun i a -> Printf.bprintf b "%s%d"
                                (if i = 0 then "" else ", ")
                                (Atomic.get a)) st.nb_connections);
  Buffer.contents b

let print_status ch st =
  output_string ch (string_status st)

type session_data = ..
type session_data += NoData

type client = {
    mutable connected : bool;
    mutable counter : int;
    mutable granularity : int;
    sock : Unix.file_descr;
    ssl  : Ssl.socket option;
    status : status;
    domain_id : int;
    session : session option
  }

and session =
  { addr : string
  ; key : string
  ; mutex : Mutex.t
  ; mutable clients : client list
  ; mutable data : session_data
  }

let fake_client =
    { counter = 0;
      granularity = 0; sock = Unix.stdout;
      status = { nb_availables = Atomic.make 0;
                 nb_connections = [||] };
      domain_id = 0;
      ssl = None;
      connected = false;
      session = None;
    }

type _ Effect.t +=
   | Read  : { sock: Unix.file_descr; fn: (unit -> int); cl: exn -> unit }
               -> int Effect.t
   | Write : { sock: Unix.file_descr; fn: (unit -> int); cl: exn -> unit }
               -> int Effect.t
   | Yield : unit Effect.t
   | Sleep : float -> unit Effect.t
   | Lock  : Mutex.t -> unit Effect.t

type action = Read|Write
type pending =
  { sock : Unix.file_descr
  ; action:action
  ; fn : unit -> int
  ; cl : exn -> unit
  ; cont : (int, unit) continuation
  ; mutable arrival_time : float (* time it started to be pending *)
  ; mutable seen_time : float    (* last time is was returned by select *)
  }

exception Closed of bool

let apply c f1 f2 =
  match c.ssl with None -> f1 c.sock
                 | Some s -> f2 s

let yield () =
  U.debug ~lvl:5 (fun k -> k "yield(1)");
  perform Yield

let now = Unix.gettimeofday

let sleep : float -> unit = fun t ->
  U.debug ~lvl:5 (fun k -> k "sleep(1,%e)" t);
  let t = now () +. t in
  perform (Sleep t)

let lock : Mutex.t -> unit = fun lk ->
  U.debug ~lvl:5 (fun k -> k "lock(1)");
  if not (Mutex.try_lock lk) then perform (Lock lk)

let close c exn =
  U.debug ~lvl:3 (fun k -> k "closing because exception: %s. connected: %b"
                             (Printexc.to_string exn) c.connected);
  if c.connected then
    begin
      Atomic.decr c.status.nb_connections.(c.domain_id);
      c.connected <- false;
      begin
        match c.session with
        | None -> ()
        | Some sess ->
           lock sess.mutex;
           sess.clients <- List.filter (fun c' -> c != c') sess.clients;
           Mutex.unlock sess.mutex
      end;
      begin
        try apply c Unix.close Ssl.shutdown with _ -> ()
      end
    end

let rec fread c s o l =
  try
    let n = apply c Unix.read Ssl.read s o l in
    U.debug ~lvl:5 (fun k -> k "read(1) %d/%d" n l);
    if n = 0 then raise (Closed true); n
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_))
     | Ssl.(Read_error(Error_want_write|Error_want_read|Error_none
                       |Error_want_x509_lookup|Error_want_accept
                       |Error_want_connect)) ->
        U.debug ~lvl:5 (fun k -> k "exn schedule write %d" l);
        perform_read c s o l
     | exn -> ignore (close c exn); raise exn

and perform_read c s o l =
  perform (Read {sock = c.sock; fn = (fun () -> read c s o l); cl = close c})

and read c s o l =
  assert(l<>0);
  c.counter <- c.counter + 1;
  if c.counter mod c.granularity = 0  &&
       (Atomic.get c.status.nb_connections.(c.domain_id) > 1 ||
          Atomic.get c.status.nb_availables <= 0) then
    begin
      U.debug ~lvl:5 (fun k -> k "normal schedule read %d" l);
      perform_read c s o l
    end
  else
    fread c s o l

let rec fwrite c s o l =
  try
    let n = apply c Unix.single_write Ssl.write s o l in
    U.debug ~lvl:5 (fun k -> k "write(1) %d/%d" n l);
    if n = 0 then raise (Closed false); n
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_))
     | Ssl.(Write_error(Error_want_write|Error_want_read|Error_none
                        |Error_want_x509_lookup|Error_want_accept
                        |Error_want_connect)) ->
        U.debug ~lvl:5 (fun k -> k "exn schedule write %d" l);
        perform_write c s o l
     | exn -> ignore (close c exn); raise exn

and perform_write c s o l =
  perform (Write {sock = c.sock; fn = (fun () -> write c s o l); cl = close c})

and write c s o l =
  assert(l<>0);
  c.counter <- c.counter + 1;
  if c.counter mod c.granularity = 0 &&
       (Atomic.get c.status.nb_connections.(c.domain_id) > 1 ||
          Atomic.get c.status.nb_availables <= 0) then
    begin
      U.debug ~lvl:5 (fun k -> k "normal schedule write %d" l);
      perform_write c s o l
    end
  else
    fwrite c s o l

let schedule_read sock fn cl =
  perform (Read {sock; fn; cl })

let schedule_write sock fn cl =
  perform (Write {sock; fn; cl })


module Io = struct
  let rec read sock s o l =
  try
    let n = Unix.read sock  s o l in
    U.debug ~lvl:5 (fun k -> k "read(1) %d/%d" n l);
    if n = 0 then raise (Closed true); n
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_))
     | Ssl.(Read_error(Error_want_write|Error_want_read|Error_none
                       |Error_want_x509_lookup|Error_want_accept
                       |Error_want_connect)) ->
        U.debug ~lvl:5 (fun k -> k "exn schedule write %d" l);
        schedule_read sock (fun () -> read sock s o l)
          (fun _ -> Unix.close sock)
     | exn -> Unix.close sock; raise exn

  let rec write sock s o l =
  try
    let n = Unix.single_write sock s o l in
    U.debug ~lvl:5 (fun k -> k "write(1) %d/%d" n l);
    if n = 0 then raise (Closed false); n
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_))
     | Ssl.(Write_error(Error_want_write|Error_want_read|Error_none
                        |Error_want_x509_lookup|Error_want_accept
                        |Error_want_connect)) ->
        U.debug ~lvl:5 (fun k -> k "exn schedule write %d" l);
        schedule_write sock (fun () -> write sock s o l)
          (fun _ -> Unix.close sock)
     | exn -> Unix.close sock; raise exn
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
  | Action of pending
  | Yield of ((unit,unit) continuation * float)
  | Lock of (unit,unit) continuation

exception TimeOut

let loop id st listens maxc granularity timeout handler () =
  let listens =
    List.map (fun l ->
        let sock = connect l.addr l.port maxc in
        (sock, l)) listens
  in
  let listen_rds = List.map fst listens in
  let pendings : (Unix.file_descr, pending) Hashtbl.t
    = Hashtbl.create 32
  in
  let yields = Queue.create () in
  let sleeps = ref [] in
  let add_sleep t cont =
    let rec fn acc = function
      | [] -> List.rev_append acc [(t,cont)]
      | (t',_)::_ as l when t < t' -> List.rev_append acc ((t,cont)::l)
      | c::l -> fn (c::acc) l
    in
    sleeps := fn [] !sleeps
  in
  let get_sleep now =
    let rec fn l =
      match l with
      | (t,cont)::l when t <= now -> Queue.add (cont,now) yields; fn l
      | l -> l
    in
    sleeps := fn !sleeps
  in
  let locks = U.LinkedList.create () in
  let add_lock lk cont = U.LinkedList.add_last (lk, cont) locks in
  let get_lock () =
    let fn (lk, _) = Mutex.try_lock lk in
    match U.LinkedList.search_and_remove_first fn locks with
    | None -> None
    | Some (_, cont) -> Some cont
  in
  let find s = try Hashtbl.find pendings s with _ -> assert false in
  let check rds wrs =
    let fn s =
      try ignore (Unix.fstat s)
      with e -> let { cl; _ } = find s in
                Hashtbl.remove pendings s;
                (try cl e with _ -> ());
    in
    List.iter fn rds;
    List.iter fn wrs
  in
  let rec poll timeout =
    let do_decr =
      if Hashtbl.length pendings = 0 then (Atomic.incr st.nb_availables; true)
      else false
    in
    let rds =
      if Atomic.get st.nb_connections.(id) < maxc then listen_rds else []
    in
    let now = now () in
    let (rds,wrs) = Hashtbl.fold (fun s c (rds,wrs as acc) ->
                        if now -. c.seen_time > timeout then
                          begin
                            Hashtbl.remove pendings s;
                            ignore (c.cl TimeOut);
                            acc
                          end
                        else
                          match c.action with
                          | Read  -> (s::rds,wrs)
                          | Write -> (rds,s::wrs)) pendings (rds,[])
    in
    get_sleep now;
    let lock_cont = get_lock () in
    let timeout =
      match Queue.is_empty yields, !sleeps with
      | false, _ -> 0.0
      | _, (t,_)::_ -> min timeout (t -. now)
      | _ -> timeout
    in
    let exception Acc of (Unix.file_descr * listenning) in
    try
      match lock_cont with
      | Some c -> Lock c  (* Mutex first!, get_lock acquire the mutex.
                             See comments in Simple_httpd.mli *)
      | None ->
         let (rds,wrs,_) = Unix.select rds wrs [] timeout in
         if do_decr then Atomic.decr st.nb_availables;
         let best = ref (match Queue.peek_opt yields with
                         | Some c -> Yield c
                         | None   -> Timeout) in
         let fn sock =
           match List.find_opt (fun (s,_) -> s == sock) listens with
           | Some l -> raise (Acc l)
           | None ->
              let {arrival_time = t';_} as p = find sock in
              p.seen_time <- now;
              match !best with
              | Timeout -> best := Action p
              | Yield(_,t) -> if t' < t then best:=Action p
              | Action{arrival_time=t;_} -> if t' < t then best:=Action p
              | Lock _ -> assert false (* rds and wrs are empty *)
              | Accept _ -> assert false
         in
         List.iter fn rds;
         List.iter fn wrs;
         !best
    with
    | Acc l -> Accept l
    | Unix.(Unix_error(EBADF,_,_)) -> check rds wrs; poll timeout
  in
  let rec do_job () =
    (try
      match poll timeout with
      | Timeout -> Domain.cpu_relax (); ()
      | Accept (lsock, linfo) ->
         U.debug (fun k -> k "accept connection from %d %a" id print_status st);
         Atomic.incr st.nb_connections.(id);
         let sock, _ = Unix.accept lsock in
         (* NOTE: non blocking is not needed, we use select.
            for ssl, this means we may block waiting for the rest of an
            encrypted block. But we non blocking seems not reliable with ssl *)
         let ssl =
           match linfo.ssl with
           | Some ctx ->
              let chan = Ssl.embed_socket sock ctx in
              Ssl.accept chan;
              Some chan
           | None ->
              None
         in
         (* TODO: we could set nonblock before accept, but in this case,
            we should deal with "retry" exceptions and schedule the retry
            of accept *)
         Unix.set_nonblock sock;
         let client = { sock; counter = 0; granularity; status = st; ssl;
           domain_id=id; connected = true; session = None } in
         handler client
      | Action { action; sock; fn; cl; cont; _ } ->
         Hashtbl.remove pendings sock;
         let n = fn () in
         U.debug ~lvl:5 (fun k -> k "%s(2) %d"
           (if action = Read then "read" else "write") n);
         if n = 0 then cl (Closed (action=Read));
         continue cont n;
      | Yield(cont,_) ->
         U.debug ~lvl:5 (fun k -> k "yield(2)");
         ignore (Queue.pop yields);
         continue cont ();
      | Lock(cont) ->
         U.debug ~lvl:5 (fun k -> k "lock(2)");
         continue cont ();

    with e ->
      U.debug (fun k -> k "exn: %s %a" (Printexc.to_string e) print_status st));
    do_job ()
  and loop () =
    try_with do_job ()
    { effc = (fun (type c) (eff: c Effect.t) ->
        match eff with
        | Yield ->
           Some (fun (cont : (c,_) continuation) ->
               Queue.add (cont, now ()) yields;
               loop ())
        | Sleep(t) ->
           Some (fun (cont : (c,_) continuation) ->
               add_sleep t cont;
               loop ())
        | Lock(lk) ->
           Some (fun (cont : (c,_) continuation) ->
               add_lock lk cont;
               loop ())
        | Read {sock; fn; cl} ->
           Some (fun (cont : (c,_) continuation) ->
               let now = now () in
               Hashtbl.add pendings sock
                 { sock; action=Read; fn; cl; cont
                 ; arrival_time = now; seen_time = now};
               loop ())
        | Write{sock; fn; cl} ->
           Some (fun (cont : (c,_) continuation) ->
               let now = now () in
               Hashtbl.add pendings sock
                 { sock; action=Write; fn; cl; cont
                 ; arrival_time = now; seen_time = now};
               loop ())
        | _ -> None
    )}
  in loop ()

let run ~nb_threads ~listens ~maxc ~granularity ~timeout handler =
  let status = {
      nb_availables = Atomic.make 0;
      nb_connections = Array.init nb_threads (fun _ -> Atomic.make 0)
    }
  in
  let fn id = spawn (loop id status listens maxc granularity timeout handler) in
  let r = Array.init (nb_threads - 1) fn in
  let _ = loop (nb_threads -1 )
            status listens maxc granularity timeout handler ()
  in
  r

let rec ssl_flush s =
  try Ssl.flush s
  with Ssl.Flush_error(true) ->
    U.debug ~lvl:0 (fun k -> k "Retry in flush");
    (* TODO: this is bad busy waiting. Should schedule like blocked read/write.
       We could not observe this exception during tests *)
    ssl_flush s

let flush c = apply c (fun _ -> ()) ssl_flush

(* All close above where because of error or socket closed on client side.
   close in Simple_httpd_server may be because there is no keep alive and
   the server close, so we flush before closing to handle the (very rare)
   ssl_flush exception above *)
let close c = flush c; close c Exit
