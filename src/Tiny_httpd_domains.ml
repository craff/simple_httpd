open Effect
open Effect.Deep
open Domain

module U = Tiny_httpd_util

type status = {
    nb_availables : int Atomic.t;
    nb_connections : int Atomic.t array
  }

let print_status ch st =
  Printf.fprintf ch "avail: %d [" (Atomic.get st.nb_availables);
  Array.iteri (fun i a -> Printf.fprintf ch "%s%d"
                           (if i = 0 then "" else ", ")
                           (Atomic.get a)) st.nb_connections;
  Printf.fprintf ch "]"

type client = {
    mutable counter : int;
    mutable granularity : int;
    sock : Unix.file_descr;
    status : status;
    id : int;
  }

type _ Effect.t +=
   | Read  : client * Bytes.t * int * int -> int Effect.t
   | Write : client * Bytes.t * int * int -> int Effect.t
   | Yield : unit Effect.t

type action = Read|Write
type pending =
  {client:client; action:action; buf:Bytes.t; offset:int; len: int;
   cont : (int,unit) continuation; mutable count:int}

exception Closed of bool

let close exn c =
  Atomic.decr c.status.nb_connections.(c.id);
  (try Unix.close c.sock with _ -> ());
  raise exn

let yield () =
  U.debug ~lvl:5 (fun k -> k "yield(1)");
  perform Yield

let read  c s o l =
  assert(l<>0);
  c.counter <- c.counter + 1;
  if c.counter mod c.granularity = 0  &&
       (Atomic.get c.status.nb_connections.(c.id) > 1 ||
          Atomic.get c.status.nb_availables <= 0) then
    (U.debug ~lvl:5 (fun k -> k "normal schedule read %d" l);
     perform (Read (c,s,o,l)))
  else
    try
      let n = Unix.read c.sock s o l in
      U.debug ~lvl:5 (fun k -> k "read(1) %d/%d" n l);
      if n = 0 then close (Closed true) c; n
    with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_)) ->
          U.debug ~lvl:5 (fun k -> k "exn schedule read %d" l);
          perform (Read (c,s,o,l))
       | exn -> close exn c

let write c s o l =
  assert(l<>0);
  c.counter <- c.counter + 1;
  if c.counter mod c.granularity = 0 &&
       (Atomic.get c.status.nb_connections.(c.id) > 1 ||
          Atomic.get c.status.nb_availables <= 0) then
    (U.debug ~lvl:5 (fun k -> k "normal schedule write %d" l);
     perform (Write(c,s,o,l)))
  else
    try
      let n = Unix.single_write c.sock s o l in
      U.debug ~lvl:5 (fun k -> k "write(1) %d/%d" n l);
      if n = 0 then close (Closed false) c; n
    with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_)) ->
          U.debug ~lvl:5 (fun k -> k "exn schedule write %d" l);
          perform (Write (c,s,o,l))
       | exn -> close exn c

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

type pollResult =
  | Timeout
  | Accept
  | Action of pending
  | Yield of ((unit,unit) continuation * int)

let loop id st addr port maxc granularity handler () =
  let listen_sock = connect addr port maxc in
  let pendings = Hashtbl.create 32 in
  let yields = Queue.create () in
  let n = ref 0 in
  let find s = try Hashtbl.find pendings s with _ -> assert false in
  let check rds wrs =
    let fn s =
      try ignore (Unix.fstat s) with _ -> Hashtbl.remove pendings s
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
      if Atomic.get st.nb_connections.(id) < maxc then [listen_sock] else []
    in
    let (rds,wrs) = Hashtbl.fold (fun s c (rds,wrs) ->
                        match c.action with
                        | Read  -> (s::rds,wrs)
                        | Write -> (rds,s::wrs)) pendings (rds,[])
    in
    let timeout = if Queue.is_empty yields then timeout else 0.0 in
    try
      let (rds,wrs,_) = Unix.select rds wrs [] timeout in
      if do_decr then Atomic.decr st.nb_availables;
      let best = ref (match Queue.peek_opt yields with
                      | None -> Timeout
                      | Some c -> Yield c) in
      let fn sock =
        if sock = listen_sock then raise Exit;
        let {count = n';_} as p = find sock in
        match !best with
        | Timeout -> best := Action p
        | Yield(_,n) ->  if n' < n then best:=Action p
        | Action{count = n;_} -> if n' < n then best:=Action p
        | Accept -> assert false
      in
      List.iter fn rds;
      List.iter fn wrs;
      !best
    with
    | Exit -> Accept
    | Unix.(Unix_error(EBADF,_,_)) -> check rds wrs; poll timeout
  in
  let rec do_job () =
    (try
      match poll 1e-3 with
      | Timeout -> Domain.cpu_relax (); ()
      | Accept ->
         U.debug (fun k -> k "accept connection from %d %a" id print_status st);
         Atomic.incr st.nb_connections.(id);
         let sock, _ = Unix.accept listen_sock in
         Unix.set_nonblock sock;
         let client = { sock; counter = 0; granularity; status = st; id } in
         handler client
      | Action { action; client; buf; offset; len; cont; _ } ->
         Hashtbl.remove pendings client.sock;
         let n =
           match action with (* can not raise EAGAIN/EWOULDBLOCK *)
           | Read  -> Unix.read client.sock buf offset len
           | Write -> Unix.single_write client.sock buf offset len
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
      U.debug (fun k -> k "exn: %s" (Printexc.to_string e)));
    do_job ()
  and loop () =
    try_with do_job ()
    { effc = (fun (type c) (eff: c Effect.t) ->
        match eff with
        | Yield ->
           Some (fun (cont : (c,_) continuation) ->
               incr n;
               Queue.add (cont, !n) yields;
               loop ())
        | Read (client,buf,offset,len) ->
           Some (fun (cont : (c,_) continuation) ->
               incr n;
               Hashtbl.add pendings client.sock
                 {client;action=Read;buf;offset;len;cont;count = !n};
               loop ())
        | Write(client,buf,offset,len) ->
           Some (fun (cont : (c,_) continuation) ->
               incr n;
               (* TODO: does not seem to work if we wait only for write ????*)
               Hashtbl.add pendings client.sock
                 {client;action=Write;buf;offset;len;cont;count = !n};
               loop ())
        | _ -> None
    )}
  in loop ()

let run nb addr port maxc granularity handler =
  if nb <= 0 || maxc < nb then
    invalid_arg "bad number of threads or max connections";
  let status = {
      nb_availables = Atomic.make 0;
      nb_connections = Array.init nb (fun _ -> Atomic.make 0)
    }
  in
  let fn id = spawn (loop id status addr port maxc granularity handler) in
  let r = Array.init (nb-1) fn in
  let _ = loop (nb-1) status addr port maxc granularity handler () in
  r
