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

type client = {
    mutable connected : bool;
    mutable counter : int;
    mutable granularity : int;
    sock : Unix.file_descr;
    ssl  : Ssl.socket option;
    status : status;
    domain_id : int;
  }

let fake_client =
    { counter = 0;
      granularity = 0; sock = Unix.stdout;
      status = { nb_availables = Atomic.make 0;
                 nb_connections = [||] };
      domain_id = 0;
      ssl = None;
      connected = false }

type _ Effect.t +=
   | Read  : client * Bytes.t * int * int -> int Effect.t
   | Write : client * Bytes.t * int * int -> int Effect.t
   | Yield : unit Effect.t
   | Sleep : float -> unit Effect.t

type action = Read|Write
type pending =
  {client:client; action:action; buf:Bytes.t; offset:int; len: int;
   cont : (int,unit) continuation;
   mutable arrival_time : float; (* time it started to be pending *)
   mutable seen_time : float;    (* last time is was returned by select *)
  }

exception Closed of bool

let apply c f1 f2 =
  match c.ssl with None -> f1 c.sock
                 | Some s -> f2 s

let close exn c =
  U.debug ~lvl:3 (fun k -> k "closing because exception: %s. connected: %b"
                             (Printexc.to_string exn) c.connected);
  if c.connected then
    begin
      Atomic.decr c.status.nb_connections.(c.domain_id);
      c.connected <- false;
      begin
        try apply c Unix.close Ssl.shutdown with _ -> ()
      end
    end;
  raise exn

let yield () =
  U.debug ~lvl:5 (fun k -> k "yield(1)");
  perform Yield

let now = Unix.gettimeofday

let sleep : float -> unit = fun t ->
  U.debug ~lvl:5 (fun k -> k "sleep(1,%e)" t);
  let t = now () +. t in
  perform (Sleep t)

let fread c s o l =
  try
    let n = apply c Unix.read Ssl.read_blocking s o l in
    U.debug ~lvl:5 (fun k -> k "read(1) %d/%d" n l);
    if n = 0 then raise (Closed true); n
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_))
     | Ssl.(Read_error(Error_want_write|Error_want_read|Error_none
                       |Error_want_x509_lookup|Error_want_accept
                       |Error_want_connect)) ->
        U.debug ~lvl:5 (fun k -> k "exn schedule write %d" l);
        perform (Read (c,s,o,l))
     | exn -> close exn c

let read c s o l =
  assert(l<>0);
  c.counter <- c.counter + 1;
  if c.counter mod c.granularity = 0  &&
       (Atomic.get c.status.nb_connections.(c.domain_id) > 1 ||
          Atomic.get c.status.nb_availables <= 0) then
    (U.debug ~lvl:5 (fun k -> k "normal schedule read %d" l);
     perform (Read (c,s,o,l)))
  else fread c s o l

let fwrite c s o l =
  try
    let n = apply c Unix.single_write Ssl.write_blocking s o l in
    U.debug ~lvl:5 (fun k -> k "write(1) %d/%d" n l);
    if n = 0 then raise (Closed false); n
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_))
     | Ssl.(Write_error(Error_want_write|Error_want_read|Error_none
                        |Error_want_x509_lookup|Error_want_accept
                        |Error_want_connect)) ->
        U.debug ~lvl:5 (fun k -> k "exn schedule write %d" l);
        perform (Write (c,s,o,l))
     | exn -> close exn c

let write c s o l =
  assert(l<>0);
  c.counter <- c.counter + 1;
  if c.counter mod c.granularity = 0 &&
       (Atomic.get c.status.nb_connections.(c.domain_id) > 1 ||
          Atomic.get c.status.nb_availables <= 0) then
    (U.debug ~lvl:5 (fun k -> k "normal schedule write %d" l);
     perform (Write(c,s,o,l)))
  else fwrite c s o l

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
  let find s = try Hashtbl.find pendings s with _ -> assert false in
  let check rds wrs =
    let fn s =
      try ignore (Unix.fstat s)
      with e ->
            let {client=c;_} = find s in
            Hashtbl.remove pendings s;
            (try close e c with _ -> ());
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
    let (rds,wrs) = Hashtbl.fold (fun s c (rds,wrs) ->
                        if now -. c.seen_time > timeout then
                          begin
                            Hashtbl.remove pendings s;
                            close TimeOut c.client
                          end
                        else
                          match c.action with
                          | Read  -> (s::rds,wrs)
                          | Write -> (rds,s::wrs)) pendings (rds,[])
    in
    get_sleep now;
    let timeout =
      match Queue.is_empty yields, !sleeps with
      | false, _ -> 0.0
      | _, (t,_)::_ -> min timeout (t -. now)
      | _ -> timeout
    in
    let exception Acc of (Unix.file_descr * listenning) in
    try
      let (rds,wrs,_) = Unix.select rds wrs [] timeout in
      if do_decr then Atomic.decr st.nb_availables;
      let best = ref (match Queue.peek_opt yields with
                      | None -> Timeout
                      | Some c -> Yield c) in
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
         Unix.set_nonblock sock; (* need to be done after Ssl_accept *)
         let client = { sock; counter = 0; granularity; status = st; ssl;
           domain_id=id; connected = true } in
         handler client
      | Action { action; client; buf; offset; len; cont; _ } ->
         Hashtbl.remove pendings client.sock;
         let n =
           (* NOTE: if using SSL, read/write can still return "want read/write"
              without SSL, we could safely call Unix.read/single_write without
              handling no data *)
           match action with
           | Read  -> fread client buf offset len
           | Write -> fwrite client buf offset len
         in
         U.debug ~lvl:5 (fun k -> k "%s(2) %d/%d"
           (if action = Read then "read" else "write") n len);
         if n = 0 then close (Closed (action=Read)) client;
         continue cont n;
      | Yield(cont,_) ->
         U.debug ~lvl:5 (fun k -> k "yield(2)");
         ignore (Queue.pop yields);
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
        | Read (client,buf,offset,len) ->
           Some (fun (cont : (c,_) continuation) ->
               let now = now () in
               Hashtbl.add pendings client.sock
                 {client;action=Read;buf;offset;len;cont;
                  arrival_time = now; seen_time = now};
               loop ())
        | Write(client,buf,offset,len) ->
           Some (fun (cont : (c,_) continuation) ->
               let now = now () in
               Hashtbl.add pendings client.sock
                 {client;action=Write;buf;offset;len;cont;
                  arrival_time = now; seen_time = now};
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

let close c = close Exit c

let flush c = apply c (fun _ -> ()) Ssl.flush