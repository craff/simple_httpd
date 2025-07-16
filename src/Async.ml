open Effect
open Effect.Deep
module LL = Util.LinkedList

let _ = Sys.catch_break false
let max_domain = Domain.recommended_domain_count () * 2

let new_id =
  let c = ref 0 in
  fun () -> let x = !c in c := x + 1; x

type any_cont =
  Cont : ('a, 'b) continuation -> any_cont
| NoCont : any_cont

exception NoRead
exception NoWrite
exception EndHandling
exception ClosedByHandler

type client =
  { id : int
  ; mutable connected : bool
  ; sock : Unix.file_descr
  ; peer : string
  ; accept_by : int (* index of the socket that accepted the connection in the
                       listens table *)
  ; mutable ssl : (Ssl.socket * bool * bool) option
  ; mutable cont : bool (* set to false to stop reading request *)
  ; mutable session : session option
  ; mutable start_time : float (* last time request started *)
  ; mutable timeout : float
  ; mutable timeout_ref : float
  ; buf : Buffer.t (* used to parse headers *)
  ; mutable last_seen_cell : client LL.cell
  ; mutable at_close : (unit -> unit) LL.t
  ; mutable read : Bytes.t -> int -> int -> int
  ; mutable write : Bytes.t -> int -> int -> int
  ; mutable sendfile : Unix.file_descr -> int -> int -> int
  ; mutable flush : unit -> unit
  }

and mutex_state =
  | Unlocked
  | Locked of client * (unit -> unit) LL.cell
  | Deleted

and mutex =
  { mutable eventfd : Unix.file_descr
  ; mutable owner   : mutex_state
  ; waiting : bool array (* which domain has added the mutex to epoll, the
                            first time it had a client blocked while trying to
                            lock *)
  }

and semaphore =
  { mutable seventfd : Unix.file_descr
  ; swaiting : bool array (* which domain has added the semaphore to epoll, the
                            first time it had a client blocked while trying to
                            lock *)
  }

and session_info =
  { addr : string
  ; key : string
  ; life_time : float
  ; clients : client list Atomic.t
  ; data : Key.data Atomic.t
  ; mutable cell : session_info Util.LinkedList.cell
  ; mutable last_refresh : float (* protected by mutex_list in Session.ml *)
  }

and session = session_info LL.cell

let fake_client =
    { sock = Unix.stdout;
      ssl = None;
      peer = "";
      connected = false;
      session = None;
      cont = true;
      id = -1;
      buf = Buffer.create 16;
      start_time = 0.0;
      timeout = 0.0;
      timeout_ref = 0.0;
      accept_by = 0;
      last_seen_cell = LL.fake_cell;
      at_close = LL.create ();
      read = (fun _ _ _ -> assert false);
      write = (fun _ _ _ -> assert false);
      sendfile = (fun _ _ _ -> assert false);
      flush = (fun _ -> assert false);
    }

let set_session ?session client =
  client.session <- session

type _ Effect.t +=
   | Io  : Unix.file_descr * bool -> unit Effect.t
   | Client  : Unix.file_descr -> unit Effect.t
   | Yield : unit Effect.t
   | Sleep : float -> unit Effect.t
   | Lock  : mutex -> unit Effect.t
   | Decr  : semaphore -> unit Effect.t

type pending = (unit, unit) continuation

let now = Unix.gettimeofday

let sleep : float -> unit = fun t ->
  let t = now () +. t in
  perform (Sleep t)

module IoTmp = struct
  type t = { sock : Unix.file_descr
           ; flags : Polly.Events.t
           ; finalise : t -> unit (** extra function when closing *)
           ; waiting : bool array
           ; closing : bool Atomic.t
           ; client : (client * (unit -> unit) LL.cell ref) option
           (* which domain has added the socket to epoll,
              the first time it had a client blocked while trying to lock *) }
end

type socket_type =
  | Io     (* Io socket *)
  | Fd     (* Bare file descriptor *)
  | Client (* main socket serving the http connexion *)
  | Pipe   (* pipe to receive the new connection *)
  | Lock   (* mutex *)
  | Decr   (* semaphore *)

let _print_socket_type = function
  | Io -> "Io"
  | Fd -> "Fd"
  | Client -> "Client"
  | Pipe -> "Pipe"
  | Lock -> "Lock"
  | Decr -> "Decr"

let printexn e =
  match e with
  | e -> (Printexc.to_string e)

type sock_error =
  | Hup
  | Err of Unix.error
  | NoError

type pending_status =
  | NoWait
  | Wait : pending -> pending_status

type socket_info =
  { ty : socket_type
  ; client : client
  ; mutable pd : pending_status
  }

type socket_infos =
  | NoSocket
  | PipeSocket of socket_info
  | ClientSocket of
      { info: socket_info; mutable evt: Polly.Events.t }
  | IoSocket of
      { qr: socket_info Queue.t
      ; qw: socket_info Queue.t
      ; mutable evt: Polly.Events.t }
  | QueueSocket of socket_info Queue.t

type pollResult =
  | Accept of (int * Unix.file_descr * Address.t)
  | Action : pending * socket_info * Polly.Events.t -> pollResult
  | Yield of ((unit,unit) continuation * client * float)
  | Poll

type domain_info =
  { mutable cur_client : client (* the client currently running *)
  ; did : Domain.id
  ; pendings : socket_infos array
  (** The main pipe of the domain and the socket of each client are
      added at creation in the table and removed went terminating
      the client.

      evenfd of [Mutex.t] and socket in [Io.t] are added to this
      table when a client is blocking on that mutex/io. As several clients
      may block on the same mutex or io, they can be present several time
      in the table of several domains.
   *)
  ; poll_list : Polly.t
  ; bytes : Bytes.t (* a preallocated buffer *)
  ; last_seen : client LL.t
  ; ready : pollResult Queue.t
  ; mutable sleeps : (float * (unit, unit) continuation * client) list
  ; nb_connections : int Atomic.t (* -1 for the accepting domain, -2 for unused domain *)
  }


let fake_domain_info =
  { cur_client = fake_client
  ; did = Domain.self ()
  ; pendings = Array.make Util.maxfd NoSocket
  ; poll_list = Polly.create ()
  ; bytes = Bytes.create 0
  ; last_seen = LL.create ()
  ; ready = Queue.create ()
  ; sleeps = []
  ; nb_connections = Atomic.make (-2)
  }

let all_domain_info = Array.make max_domain fake_domain_info

let global_get_dinfo () =
  let id = Domain.self () in
  all_domain_info.((id :> int))
let global_get_client () =
  (global_get_dinfo()).cur_client

module Log = struct
  type log_lvl =
    | Req of int
    | Sch of int
    | Exc of int
    | Aut of int
    | Prc of int
    | Usr of int

  type log_status =
    { mutable requests : int
    ; mutable scheduler : int
    ; mutable exceptions : int
    ; mutable authentications : int
    ; mutable processes : int
    ; mutable user : int }

  let log_status =
    { requests = 1
    ; scheduler = 0
    ; exceptions = 1
    ; authentications = 1
    ; processes = 1
    ; user = 1 }

  let set_log_requests        n = log_status.requests        <- n
  let set_log_scheduler       n = log_status.scheduler       <- n
  let set_log_exceptions      n = log_status.exceptions      <- n
  let set_log_authentications n = log_status.authentications <- n
  let set_log_processes       n = log_status.processes       <- n
  let set_log_user            n = log_status.user            <- n

  let do_log = function
    | Req n -> n < log_status.requests
    | Sch n -> n < log_status.scheduler
    | Exc n -> n < log_status.exceptions
    | Aut n -> n < log_status.authentications
    | Prc n -> n < log_status.processes
    | Usr n -> n < log_status.user

  let str_log = function
    | Req n -> "Req", n
    | Sch n -> "Sch", n
    | Exc n -> "Exc", n
    | Aut n -> "Aut", n
    | Prc n -> "Prc", n
    | Usr n -> "Usr", n

  let log_files = ref [||]
  let log_folder = ref ""
  let log_basename = ref ""
  let log_perm = ref 0o700

  let fname i = Filename.concat !log_folder
                      (!log_basename ^ "-" ^ string_of_int i ^ ".log")

  let open_log i =
    let filename = fname i in
    let ch = open_out_gen [Open_wronly; Open_append; Open_creat] !log_perm filename in
    let fd = Unix.descr_of_out_channel ch in
    let stat = Unix.fstat fd in
    let ctime = stat.st_ctime in
    let size = stat.st_size in
    if size <= 0 then
      Printf.fprintf ch "%.6f %2d %10d Log0: log created\n%!"
        (Unix.gettimeofday ()) i (-1);
    ch, Format.formatter_of_out_channel ch, ctime

  let get_log id =
    let open Unix in
    let ch, fmt, ctime = !log_files.(id) in
    (* reopen log file it no link: logrotate might delete it*)
    let reopen () =
      (try close_out ch with Unix_error _ | Sys_error _ -> ());
      let _, fmt, _ as c = open_log id in
      !log_files.(id) <- c;
      fmt
    in
    try
      let stat = fstat (descr_of_out_channel ch) in
      if ctime >= 0.0 && (stat.st_nlink < 1 || ctime <> stat.st_ctime)
      then reopen () else fmt
    with Unix_error _ -> reopen ()


  let set_log_folder ?(basename="log") ?(perm=0o700) folder nb_dom =
    log_perm := perm;
    log_basename := basename;
    log_folder := folder;
    if not (Sys.file_exists folder) then Sys.mkdir folder perm;
    if not (Sys.is_directory folder) then invalid_arg "set_log_folder";
    try
      let a = Array.init nb_dom (function i -> open_log i) in
      log_files := a
    with e -> failwith ("set_log_folder: " ^ Printexc.to_string e)

  let init_log_folder nb_dom =
    if Array.length !log_files < nb_dom then
      begin
        log_files :=
          Array.init nb_dom
            (fun _ -> let stdout = Unix.out_channel_of_descr Unix.stdout in
                      (stdout, Format.formatter_of_out_channel stdout, -1.0))
      end

  let f ty k =
    if do_log ty then (
      k (fun fmt->
          let id = Domain.((self() :> int)) in
          let cl = global_get_client () in
          let ch = get_log id in
          let log,lvl = str_log ty in
          Format.fprintf ch "%.6f %2d %10d %s%d: "
            (Unix.gettimeofday ()) id cl.id log lvl;
          Format.kfprintf (fun oc ->
              try
                Format.fprintf oc "\n%!"
              with
                Sys_error _ | Unix.Unix_error _ -> ()) ch fmt
        )
    )

  let fname id =
    let _ = get_log id in
    fname id

  let _ = Address.forward_log := f (Exc 0)
end

let yield () =
  let q = all_domain_info.((Domain.self () :> int)).nb_connections in
  if Atomic.get q > 1 then perform Yield

let remove_at_close client cell =
  LL.remove_cell cell client.at_close

let at_close client action =
  let ptr = ref LL.fake_cell in
  let action () =
    remove_at_close client !ptr;
    action ()
  in
  let res = LL.add_first action client.at_close in
  ptr := res;
  res

module Mutex : sig
    type t = mutex
    val create : unit -> t
    val unlock : t -> unit
    val try_lock : t -> bool
    val lock : t -> unit
    val delete : t -> unit
    val try_unlock : t -> unit
  end = struct
  external raw_eventfd  : int -> int -> Unix.file_descr = "caml_eventfd"
  (*external raw_efd_cloexec   : unit -> int = "caml_efd_cloexec"*)
  external raw_efd_nonblock  : unit -> int = "caml_efd_nonblock"

  let flags = raw_efd_nonblock()

  type t = mutex

  let delete r =
    (* FIXME: max_domain too large *)
    let action () =
      for i = 0 to max_domain - 1 do
        if r.waiting.(i) then
          all_domain_info.(i).pendings.(Util.file_descr_to_int r.eventfd) <- NoSocket
      done;
       r.owner <- Deleted;
       (try Unix.close r.eventfd with Unix.Unix_error _ -> ())
    in
    match r.owner with
    | Deleted -> ()
    | Locked _ ->
       Log.(f (Exc 0)) (fun k -> k "Mutex collected before unlock!");
       action ()
    | Unlocked ->
       action ()

  let create () =
    let r =
      { eventfd = raw_eventfd 1 flags
      ; owner = Unlocked
      ; waiting = Array.make max_domain false}
    in
    Gc.finalise delete r;
    r

  let one = let r = Bytes.create 8 in Bytes.set_int64_ne r 0 1L; r

  let unlock lk =
    begin
      match lk.owner with
      | Locked (cl, cell) ->
         remove_at_close cl cell
      | Unlocked ->
         failwith "unlock a not locked mutex"
      | Deleted ->
         failwith "unlock a deleted mutex"
    end;
    lk.owner <- Unlocked;
    assert (Util.single_write lk.eventfd one 0 8 = 8)

  let try_unlock lk =
    match lk.owner with
    | Locked (cl,_) when global_get_client () = cl -> unlock lk
    | _ -> ()

  let try_lock lk =
    if lk.owner = Deleted then failwith "lock a deleted mutex";
    let buf = Bytes.create 8 in
    try assert(Util.read lk.eventfd buf 0 8 = 8);
        let cl = global_get_client () in
        let cell = at_close cl (fun () -> unlock lk) in
        lk.owner <- Locked (cl, cell);
        true
    with Unix.(Unix_error((EAGAIN | EWOULDBLOCK), _, _)) -> false

  let rec lock : t -> unit = fun lk ->
    if not (try_lock lk) then (perform (Lock lk); lock lk)
end

module Semaphore : sig
    type t = semaphore
    val create : int -> t
    val decr : Mutex.t -> t -> unit
    val try_decr : t -> bool
    val incr : t -> unit
    val delete : t -> unit
  end = struct
  external raw_eventfd  : int -> int -> Unix.file_descr = "caml_eventfd"
  (*external raw_efd_cloexec   : unit -> int = "caml_efd_cloexec"*)
  external raw_efd_nonblock  : unit -> int = "caml_efd_nonblock"
  external raw_efd_semaphore  : unit -> int = "caml_efd_semaphore"

  let flags = raw_efd_nonblock() lor raw_efd_semaphore ()

  type t = semaphore

  let delete r =
    for i = 0 to max_domain - 1 do
      if r.swaiting.(i) then
        all_domain_info.(i).pendings.(Util.file_descr_to_int r.seventfd) <- NoSocket
    done;
    try Unix.close r.seventfd with Unix.Unix_error _ -> ()

  let create n =
    let r =
      { seventfd = raw_eventfd n flags
      ; swaiting = Array.make max_domain false}
    in
    Gc.finalise delete r;
    r

  let one = let r = Bytes.create 8 in Bytes.set_int64_ne r 0 1L; r

  let incr lk =
    assert (Util.single_write lk.seventfd one 0 8 = 8)

  let try_decr lk =
    let buf = Bytes.create 8 in
    try
        ignore (Util.read lk.seventfd buf 0 8 = 8);
        true
    with Unix.(Unix_error((EAGAIN | EWOULDBLOCK), _, _)) -> false

  let rec decr : Mutex.t -> t -> unit = fun m lk ->
    Mutex.lock m;
    if not (try_decr lk) then (Mutex.unlock m; perform (Decr lk); decr m lk);
end

module Ssl = struct include Ssl include Ssl.Runtime_lock end

let read_unix fd =
  let rec fn s o l =
    try
      Util.read fd s o l
    with
    | Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_)) ->
       perform (Client fd);
       fn s o l
  in
  fn

let read_ssl fd chan =
  let rec fn s o l =
    try
      Ssl.read chan s o l
    with
    | Ssl.(Read_error(Error_want_read|Error_want_write)) ->
       perform (Client fd);
       fn s o l
  in
  fn

let write_unix fd =
  let rec fn s o l =
    try
      Util.single_write fd s o l
    with
    | Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_)) ->
       perform (Client fd);
       fn s o l
  in
  fn

let write_ssl fd chan =
  let rec fn s o l =
    try
      Ssl.write chan s o l
    with Ssl.(Write_error(Error_want_read|Error_want_write)) ->
      perform (Client fd);
        fn s o l
  in
  fn

let sendfile_unix fd =
  let rec fn in_fd in_off l =
    try
      Util.sendfile fd in_fd in_off l
    with
    | Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_)) ->
      perform (Client fd);
      fn in_fd in_off l
  in
  fn

(* Note: sendfile together with SSL, not really efficient,
   unless ktls is used *)
let ssl_sendfile c fd o l =
  let buf = Bytes.create 0x4000 in (* 16K: the default for SSL*)
  let len = Bytes.length buf in
  let _ = Unix.(lseek fd o SEEK_SET) in
  let w = Unix.read fd buf 0 (min l len) in
  Ssl.write c buf 0 w

let sendfile_ssl fd chan =
  let rec fn in_fd in_off l =
    try
      ssl_sendfile chan in_fd in_off l
    with
    | Ssl.(Write_error(Error_want_read|Error_want_write)) ->
       perform (Client fd);
       fn in_fd in_off l
  in

  fn

let schedule_fd read sock = perform (Io (sock, read))

let flush_unix fd () =
  Util.flush_cork fd

let flush_ssl fd chan =
  let rec fn () =
    try Ssl.flush chan
    with Ssl.Flush_error(true) ->
      schedule_fd false fd;
      fn ()
  in
  fn

let register_starttime cl =
  let r = now () in
  cl.start_time <- r;
  cl.timeout_ref <- r;
  let ll = all_domain_info.((Domain.self () :> int)).last_seen in
  Util.LinkedList.move_first cl.last_seen_cell ll;
  r

let stop_client (cl:client) =
  cl.cont <- false

let reset_timeout cl =
  let r = now () in
  cl.timeout_ref <- r;
  let ll = all_domain_info.((Domain.self () :> int)).last_seen in
  Util.LinkedList.move_first cl.last_seen_cell ll

let create_io () =
  IoSocket{ qr = Queue.create ()
          ; qw = Queue.create ()
          ; evt = Polly.Events.empty }

let register_fd fd flags =
  let i = (Domain.self () :> int) in
  let info = all_domain_info.(i) in
  Polly.add info.poll_list fd flags;
  info.pendings.(Util.file_descr_to_int fd) <- create_io ()

let unregister_fd fd =
  let i = (Domain.self () :> int) in
  let info = all_domain_info.(i) in
  info.pendings.(Util.file_descr_to_int fd) <- NoSocket;
  Polly.del info.poll_list fd

module Io = struct
  include IoTmp

  let register (r : t) =
    let i = (Domain.self () :> int) in
    let info = all_domain_info.(i) in

    begin
      match r.waiting.(i), info.pendings.(Util.file_descr_to_int r.sock)  with
      | false, NoSocket ->
         r.waiting.(i) <- true;
         Polly.(add info.poll_list r.sock r.flags)
      | false, _ ->
         r.waiting.(i) <- true;
      | _ -> ()
    end;
    info.pendings.(Util.file_descr_to_int r.sock) <- create_io ()

  let unregister (r : t) =
    let i = (Domain.self () :> int) in
    let info = all_domain_info.(i) in
    if r.waiting.(i) then begin
        r.waiting.(i) <- false;
        (* If the fd is closed in finalise, this is not needed *)
        try Polly.(del info.poll_list r.sock) with Unix.Unix_error _ -> ()
      end;
    info.pendings.(Util.file_descr_to_int r.sock) <- NoSocket

  let close (s:t) =
    if Atomic.compare_and_set s.closing false true then
      begin
        unregister s;
        (match s.client with
        | None -> ()
        | Some (cl, { contents = cell }) -> LL.remove_cell cell cl.at_close);
        s.finalise s;
      end

  let create
        ?(edge_triggered=true)
        ?(finalise=fun io ->
                   (try Unix.(close io.sock); with Unix.Unix_error _ -> ()))
        ?client
        sock =
    let flags = Polly.Events.(inp lor out lor err lor hup) in
    let flags = if edge_triggered then Polly.Events.(et lor flags) else flags in
    let ptr = ref LL.fake_cell in
    let client = match client with
      | None -> None
      | Some c -> Some (c, ptr)
    in
    let r = { sock
            ; finalise
            ; flags
            ; waiting = Array.make max_domain false
            ; closing = Atomic.make false
            ; client }
    in
    ptr := LL.new_cell (fun () -> close r);
    register r;
    Gc.finalise close r;
    begin
      match client with
      | None -> ()
      | Some (c, _) -> LL.insert_first !ptr c.at_close
    end;
    r

  let schedule read io = schedule_fd read io.sock

  let rec read (io:t) s o l =
    try
      Util.read io.sock  s o l
    with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_)) ->
      schedule_fd true io.sock;
      read io s o l

  let rec write (io:t) s o l =
    try
      Util.single_write io.sock s o l
    with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_)) ->
      schedule_fd false io.sock;
      write io s o l

  let flush (io:t) =
    Util.flush_cork io.sock

  let formatter (io:t) =
    let open Format in
    let out_string s o l = ignore (write io (Bytes.unsafe_of_string s) o l) in
    let funs = {
        out_string;
        out_flush = (fun () -> ());
        out_newline = (fun () -> out_string "\n" 0 1);
        out_spaces = (fun n -> out_string (String.make n ' ') 0 n);
        out_indent = (fun n -> out_string (String.make n ' ') 0 n);
      }
    in
    formatter_of_out_functions funs

  let sock io = io.sock

end

let is_ipv6 addr = String.contains addr ':'

let connect addr port reuse maxc =
  ignore (Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigpipe] : _ list);
  let sock =
    Unix.socket
      (if is_ipv6 addr then Unix.PF_INET6 else Unix.PF_INET)
      Unix.SOCK_STREAM
      0
  in
  try
    Unix.set_close_on_exec sock;
    Unix.set_nonblock sock;
    Unix.(setsockopt_optint sock SO_LINGER None);
    Unix.(setsockopt sock SO_REUSEADDR reuse);
    let inet_addr = Unix.inet_addr_of_string addr in
    Unix.bind sock (Unix.ADDR_INET (inet_addr, port));
    Unix.listen sock maxc;
    sock
  with e -> (try Unix.close sock; with _ -> ()); raise e

let _print_pollResult ch r = match r with
  | Yield _ -> Format.fprintf ch "Yield"
  | Accept _ -> Format.fprintf ch "Accept"
  | Action _ -> Format.fprintf ch "Action"
  | Poll -> Format.fprintf ch "Poll"

exception FailHandling

let add_ready dinfo x =
  Queue.add x dinfo.ready

(* Managment of sleep *)
(* O(N) when N is the current number of sleep. Could use Map ?*)
let add_sleep dinfo t cont =
  let cl = dinfo.cur_client in
  let rec fn acc = function
    | [] -> List.rev_append acc [(t,cont,cl)]
    | (t',_,_)::_ as l when t < t' -> List.rev_append acc ((t,cont,cl)::l)
    | c::l -> fn (c::acc) l
  in
  dinfo.sleeps <- fn [] dinfo.sleeps

(* amortized O(1) *)
let get_sleep dinfo now =
  let rec fn l =
    match l with
    | (t,cont,cl)::l when t <= now ->
       if cl.connected then
         begin
           add_ready dinfo (Yield(cont,cl,now));
         end;
       fn l
    | l -> l
  in
  dinfo.sleeps <- fn dinfo.sleeps

let en_queue dinfo socket info =
  let fd = Util.file_descr_to_int socket in
  let q =
    match dinfo.pendings.(fd) with
    | NoSocket ->
       let q = Queue.create () in
       dinfo.pendings.(fd) <- QueueSocket q;
       q
    | QueueSocket q -> q
    | ClientSocket _ -> assert false
    | IoSocket _ -> assert false
    | PipeSocket _ -> assert false
  in

  Queue.add info q

let add_lock : domain_info -> Mutex.t -> (unit, unit) continuation -> unit =
  fun dinfo lk cont ->
  let cl = dinfo.cur_client in
  let did = dinfo.did in
  let info = { ty = Lock ; client = cl ; pd = Wait cont } in
  (* may be another client is already waiting *)
  if not lk.waiting.((did :> int)) then
    begin
      Polly.(add dinfo.poll_list lk.eventfd Events.(inp lor et));
      lk.waiting.((did :> int)) <- true
    end;

  en_queue dinfo lk.eventfd info


let add_decr : domain_info -> Semaphore.t -> (unit, unit) continuation -> unit =
  fun dinfo lk cont ->
  let cl = dinfo.cur_client in
  let did = dinfo.did in
  let info = { ty = Decr ; client = cl ; pd = Wait cont }
  in
  (* may be another client is already waiting *)
  if not lk.swaiting.((did :> int)) then
    begin
      Polly.(add dinfo.poll_list lk.seventfd Events.(inp lor et));
      lk.swaiting.((did :> int)) <- true
    end;

  en_queue dinfo lk.seventfd info

let spawn dinfo client f =
  match f () with
  | () -> ()
  | effect Yield, cont ->
     add_ready dinfo (Yield(cont, client, now ()))
  | effect Sleep t, cont ->
     add_sleep dinfo t cont
  | effect Lock(lk), cont ->
     add_lock dinfo lk cont
  | effect Decr(lk), cont ->
     add_decr dinfo lk cont
  | effect Client(sock), cont ->
     let info = dinfo.pendings.(Util.file_descr_to_int sock) in
     begin
       match info with
       | NoSocket | IoSocket _ | QueueSocket _ | PipeSocket _ -> assert false
       | ClientSocket ({info; evt } as r) ->
          match info.pd with
          | NoWait ->
             let open Polly.Events in
             let e =
               let e = evt land (inp lor out) in
               if e <> empty then e else evt
             in
             if e <> empty then
               add_ready dinfo (Action(cont, info, e))
             else
               info.pd <- Wait cont;
             r.evt <- evt land (hup lor err);
          | Wait _ -> assert false
     end
  | effect Io(sock,read), cont ->
     let qs = dinfo.pendings.(Util.file_descr_to_int sock) in
     begin
       match qs with
       | NoSocket ->  assert false
       | ClientSocket _ -> assert false
       | QueueSocket _ -> assert false
       | PipeSocket _ -> assert false
       | IoSocket ({qr; qw; evt} as r) ->
          let open Polly.Events in
          let q, e0, e1 = if read then (qr, inp, out) else (qw, out, inp) in
          r.evt <- evt land (e1 lor hup lor err);
          let e = if evt land e0 <> empty then e0 else (evt land (lnot e1)) in
          let info = { ty = Io ; client ; pd = Wait cont } in
          Queue.add info q;
          if e <> empty then
            begin
              let info = Queue.take q in (* Never empty ! *)
              info.pd <- NoWait;
              add_ready dinfo (Action(cont, info, e))
            end
     end
  | exception e ->
     Log.f (Exc 0) (fun k -> k "Exception in spawn %s"
                               (Printexc.to_string e))

let close ~dinfo ~client exn =
  if client.connected then begin
    dinfo.cur_client <- client;
    Log.f (Exc 0) (fun k -> k "Closing client on exception %s"
                              (Printexc.to_string exn));
    LL.iter (fun f -> f ()) client.at_close;
    LL.remove_cell client.last_seen_cell dinfo.last_seen;
    Atomic.decr dinfo.nb_connections;
    client.connected <- false;
    dinfo.pendings.(Util.file_descr_to_int client.sock) <- NoSocket;
    (try Unix.close client.sock with Unix.Unix_error _ -> ());

    match client.session with
    | None -> ()
    | Some sess ->
       let sess = LL.get sess in
       let fn clients =
         try Util.remove_first (fun c' -> client == c') clients
         with Not_found -> assert false
       in
       let remain = Util.update_atomic sess.clients fn in
       if remain = [] then
         ignore (Util.update_atomic sess.data Key.cleanup_filter);
       ()
    end

exception Switch

let loop listens pipe timeout handler () =
  let did = Domain.self () in

  let poll_list = Polly.create () in
  Polly.(add poll_list pipe Events.(inp lor et));
  (* size for two ints *)
  let pipe_buf = Bytes.create 8 in
  (* table of all sockets *)
  let pendings = Array.make Util.maxfd NoSocket in
  let pipe_info = { ty = Pipe; client = fake_client; pd = NoWait } in
  pendings.(Util.file_descr_to_int pipe) <- PipeSocket pipe_info;

  let bytes = Bytes.create (16 * 4_096) in
  let last_seen = LL.create () in
  let ready = Queue.create () in
  let dinfo = { cur_client = fake_client; did
              ; pendings ; poll_list; bytes
              ; last_seen; ready; sleeps = []
              ; nb_connections = Atomic.make 0} in
  all_domain_info.((did :> int)) <- dinfo;


  (* Queue for ready sockets *)
  add_ready dinfo Poll; (* Invariant: queue as always exactly one Poll
                           and is never empty, except during poll. *)

  (* Managment of lock *)
  (* O(1) *)

  let iter_pendings e fn s =
    let fd = Util.file_descr_to_int s in
    match pendings.(fd) with
    | NoSocket -> ()
    | ClientSocket {info; evt} -> fn info Polly.Events.(e lor evt)
    | PipeSocket info -> fn info e
    | QueueSocket q ->
       begin
         match Queue.take_opt q with
         | None -> ()
         | Some i -> fn i e
       end
    | IoSocket ({qr; qw; evt } as r) ->
       let open Polly.Events in
       let evt = e lor evt in
       if e land (inp lor hup lor err) <> empty then
         begin
           match Queue.take_opt qr with
           | None -> ()
           | Some i ->
              fn i (evt land (lnot out));
              r.evt <- evt land (lnot inp);
         end;
       if e land (out lor hup lor err) <> empty then
         begin
           match Queue.take_opt qw with
           | None -> ()
           | Some i ->
              fn i (evt land (lnot inp));
              r.evt <- evt land (lnot out);
         end
  in

  let rec remove_timeout now =
    try
      let cell = LL.tail last_seen in
      let client = LL.get cell in
      if now -. client.timeout_ref > client.timeout then
        begin
          close ~dinfo ~client Unix.(Unix_error (ETIMEDOUT, "", ""));
          remove_timeout now
        end
    with Not_found -> ()
  in

  let rec poll () =
    let now = now () in
    try
      (* O(n) when n is the number of waiting lock *)
      get_sleep dinfo now;
      remove_timeout now;
      let select_timeout =
        match Queue.is_empty ready, dinfo.sleeps with
        | false, _ -> 0.0
        | _, (t,_,_)::_ -> min timeout (t -. now +. 1e-3)
        | _ -> timeout
      in
      let select_timeout = int_of_float (1e3 *. select_timeout) in
      let fn _ sock evt =
        let gn info evt =
          match info with
          | { ty = Pipe; _ } ->
             begin
               try
                 while true do
                   assert (Util.read pipe pipe_buf 0 8 = 8);
                   let sock : Unix.file_descr =
                     Util.file_descr_of_int
                       (Int32.to_int (Bytes.get_int32_ne pipe_buf 0))
                   in
                   let index = Int32.to_int (Bytes.get_int32_ne pipe_buf 4) in
                   let l = listens.(index) in
                   add_ready dinfo (Accept (index, sock, l))
                 done
               with Unix.Unix_error((EAGAIN|EWOULDBLOCK),_,_) -> ()
                  | e -> Log.f (Exc 0) (fun k -> k "unexpected accept recv: %s"
                                                   (Printexc.to_string e))
             end
          | { pd = NoWait ; _ } -> ()
          | { pd = Wait cont ; _ } as p ->
             add_ready dinfo (Action(cont,p,evt));
             p.pd <- NoWait
        in
        iter_pendings evt gn sock
      in
      Log.f (Sch 0) (fun k -> k "entering poll with timeout: %d (%d conn.)"
                                select_timeout
                                (Atomic.get (dinfo.nb_connections)));
      ignore (Polly.wait poll_list 1000 select_timeout fn);
    with
    | Unix.(Unix_error(EINTR,_,_)) -> poll ()
    | exn -> Log.f (Exc 0) (fun k -> k "unexpected poll: %s\n%!" (printexn exn));
             poll ()
  in
  let step () =
      match Queue.take ready with
      | Poll ->
         poll ();
         add_ready dinfo Poll

      | Accept (index, sock, linfo) ->
         let client =
           try
             let peer = Util.addr_of_sock sock in
             let now = now () in
             let client = { sock; ssl = None; id = new_id ()
                            ; peer
                            ; connected = true; session = None; cont = true
                            ; start_time = now; timeout_ref = now
                            ; timeout
                            ; buf = Buffer.create 4_096
                            ; accept_by = index; last_seen_cell = LL.fake_cell
                            ; at_close = LL.create ()
                            ; read = read_unix sock
                            ; write = write_unix sock
                            ; sendfile = sendfile_unix sock
                            ; flush = flush_unix sock
                          }
             in
             let info = { ty = Client; client; pd = NoWait } in
             dinfo.cur_client <- client;
             client.last_seen_cell <- LL.add_first client dinfo.last_seen;
             let evt = Polly.Events.empty in
             pendings.(Util.file_descr_to_int sock)
               <- ClientSocket {info; evt};
             client
           with e ->
             Log.f (Exc 0) (fun k -> k "socket closed before accepting: %s"
                                       (Printexc.to_string e));
             Unix.(try close sock with Unix_error _ -> ());
             Atomic.decr dinfo.nb_connections;
             raise FailHandling
         in
         spawn dinfo client (fun () ->
             try
               Log.f (Req 0)
                 (fun k -> k "[%d] accepted connection from %s" client.id client.peer);
               Unix.set_nonblock sock;
               Polly.(add poll_list sock Events.(inp lor out lor et lor hup lor err));
               begin
                 match linfo.ssl with
                 | Some ctx ->
                    let chan = Ssl.embed_socket sock ctx in
                    let rec fn () =
                      try
                        Ssl.accept chan; 1
                      with
                      | Ssl.(Accept_error(Error_want_read|Error_want_write|Error_want_accept)) ->
                         perform (Client sock);
                         fn ()
                    in
                    ignore (fn ());
                    let n = Util.check_ktls chan in
                    client.ssl <- Some (chan, n land 0x1 <> 0, n land 0x2 <> 0);
                    if n land 0x1 = 0 then
                      begin
                        client.write <- write_ssl sock chan;
                        client.sendfile <- sendfile_ssl sock chan;
                        client.flush <- flush_ssl sock chan;
                      end
                    else
                      begin
                        Util.setsockopt_cork sock true;
                      end;
                    if n land 0x2 = 0 then
                      begin
                        client.read <- read_ssl sock chan
                      end;
                    Log.f (Req 0) (fun k ->
                        let open Ssl in
                        let [@ocaml.warning "-3"] v = match Ssl.version chan with
                          | SSLv23 -> "SSL 2.3"
                          | SSLv3 -> "SSL 3.0"
                          | TLSv1 -> "TLS 1.0"
                          | TLSv1_1 -> "TLS 1.1"
                          | TLSv1_2 -> "TLS 1.2"
                          | TLSv1_3 -> "TLS 1.3"
                        in
                        let cn =
                          Ssl.(get_cipher_name (get_cipher chan))
                        in
                        let cv =
                          Ssl.(get_cipher_version (get_cipher chan))
                        in
                        k "ssl connection established (%s, cipher: %s %s, ktls: %d)"
                          v cn cv n);
                 | None ->
                    Util.setsockopt_cork sock true

               end;
               handler client;
               close ~dinfo ~client EndHandling
             with
             | Switch ->
                Log.f (Exc 0) (fun k -> k "Switching protocol");
             | e ->
                close ~dinfo ~client e)
      | Action (cont, p, e) ->
         let hup = Polly.Events.((hup land e) <> empty) in
         let e =
           if Polly.Events.((err land e) <> empty) then Err Unix.EPIPE
           else if hup && Polly.Events.(((out lor inp) land e) = empty) then Hup
           else NoError
         in
         let cl = p.client in
         if cl.connected then
           begin
             dinfo.cur_client <-cl;
             match e with
             | Hup ->
                discontinue cont Unix.(Unix_error (EPIPE,"HUP",""))
             | Err _ ->
                discontinue cont Unix.(Unix_error (EPIPE,"ERR",""))
             | NoError ->
                continue cont ()
           end
      | Yield(cont,cl,_) ->
         if cl.connected then
           begin
             dinfo.cur_client <- cl;
             continue cont ();
           end


  in
  let rec inner_loop () =
    try
      while true do
        step ()
      done
    with
    | FailHandling -> inner_loop ()
    | e -> Log.f (Exc 0) (fun k -> k "UNEXPECTED EXCEPTION IN INNER LOOP: %s"
                                     (Printexc.to_string e));
           inner_loop ()
  in
  inner_loop ()


let add_close, close_all =
  let to_close = ref [] in
  let add_close s = to_close := s :: !to_close in
  let close_all _ =
    List.iter
      (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ())
      !to_close;
  in
  (add_close, close_all)

let accept_loop (domains : Domain.id Array.t) listens pipes maxc =
  let exception Full in
  let poll_list = Polly.create () in
  let nb_listens = Array.length listens in
  let nb_pipes = Array.length pipes in
  let tbl = Hashtbl.create (nb_listens * 4) in
  let pipes_tbl = Hashtbl.create (nb_pipes * 4) in
  let pipe_buf = Bytes.create 8 in
  let pendings = Queue.create () in
  let blocked = Array.make nb_pipes false in
  let nbc i =  all_domain_info.((domains.(i) :> int)).nb_connections in
  Array.iteri (fun i (s,_) ->
      add_close s;
      Hashtbl.add tbl s i;
      Polly.(add poll_list s Events.(inp lor et))) listens;
  Array.iteri (fun i s ->
      Hashtbl.add pipes_tbl s i) pipes;

  let get_best () =
    let index = ref 0 in
    let c = ref max_int in
    let t = ref 0 in
    for i = 0 to nb_pipes - 1 do
      let c' = Atomic.get (nbc i) in
      t := !t + c';
      if c' < !c && not blocked.(i) then (index := i; c := c')
    done;
    if !t >= maxc || !c = max_int then raise Full;
    (!index, pipes.(!index))
  in
  let send_socket sock lsock =
    let index = try Hashtbl.find tbl sock with Not_found -> assert false in
    try
    let (pipe_index, pipe) = get_best () in
    try
      Bytes.set_int32_ne pipe_buf 0 (Int32.of_int (Util.file_descr_to_int lsock));
      Bytes.set_int32_ne pipe_buf 4 (Int32.of_int index);
      assert(Util.single_write pipe pipe_buf 0 8 = 8);
      Atomic.incr (nbc pipe_index);
    with
    | Unix.(Unix_error ((EAGAIN |EWOULDBLOCK), _,_)) ->
       blocked.(pipe_index) <- true;
       Polly.(add poll_list pipe Events.(out lor oneshot));
       Queue.add (sock, lsock) pendings
    | exn ->
       Log.f (Exc 0) (fun k -> k "unexpected exception in accept loop: %s" (printexn exn));
       (try Unix.close lsock with Unix.Unix_error _ -> ())
    with
    | Full ->
       Log.f (Exc 0) (fun k -> k "handler: reject too many clients");
       (try Unix.close lsock with Unix.Unix_error _ -> ())
  in
  let treat _ sock evt =
    try
      while true do
        if Polly.Events.(inp land evt <> empty) then
          begin
            let lsock, _ =
              try Unix.accept ~cloexec:true sock with
              | Unix.(Unix_error ((EAGAIN |EWOULDBLOCK), _,_)) ->
                 raise Exit
              | Unix.Unix_error _ as exn ->
                 Log.f (Exc 0) (fun k -> k "unexpected exception in Unix.accept: %s" (printexn exn));
                 raise FailHandling
            in
            send_socket sock lsock;
          end
        else
          begin
            let pipe_index = try Hashtbl.find pipes_tbl sock with Not_found -> assert false in
            if blocked.(pipe_index) then blocked.(pipe_index) <- false;
            try
              while not (Array.for_all (fun x -> x) blocked) do
                let (sock, lsock) = Queue.pop pendings in
                send_socket sock lsock;
              done
            with Queue.Empty -> ()
          end
      done
    with Exit -> ()
  in
  let nb_socks = Array.length listens in
  while true do
    try
      ignore (Polly.wait poll_list nb_socks 60_000 treat)
    with
    | Unix.(Unix_error(EINTR,_,_)) -> ()
    | exn ->
       Log.f (Exc 0) (fun k -> k "unexpected exception accept loop: %s" (printexn exn))
  done

type 'a result = NoResult | Ok of 'a | Exn of exn

let spawn f =
  let dinfo = global_get_dinfo () in
  let client = dinfo.cur_client in
  let result = ref NoResult in
  let m = Mutex.create () in
  let cell = at_close client (fun () -> Mutex.delete m) in
  let f () =
    Mutex.lock m;
    try
      result := Ok (f ());
      remove_at_close client cell;
      Mutex.delete m;
    with e ->
      Log.f (Sch 0) (fun k -> k "exception in spawn %s" (Printexc.to_string e));
      result := Exn e;
      remove_at_close client cell;
      Mutex.delete m
  in
  let wait () =
    (try Mutex.lock m with Failure _ -> ());
    match !result with
    | NoResult -> assert false
    | Ok v -> Result.Ok v
    | Exn e -> Result.Error e
  in
  spawn dinfo (dinfo.cur_client) f;
  wait


let run ~nb_threads ~listens ~maxc ~timeout ~set_domains handler =
  let open Address in
  let listens =
    Array.map (fun l ->
        let sock = connect l.addr l.port l.reuse maxc in
        (sock, l)) listens
  in
  let pipes = Array.init nb_threads (fun _ -> Unix.pipe ()) in
  let listens_r = Array.map snd listens in
  let fn id =
    let (r, _) = pipes.(id) in
    Unix.set_nonblock r;
    Domain.spawn (loop listens_r r timeout handler)
  in
  let pipes = Array.map snd pipes in
  let _ = Array.iter Unix.set_nonblock pipes in
  let r = Array.init nb_threads fn in
  let domains = Array.map (fun d -> Domain.get_id d) r in
  set_domains domains;
  try
    let _ = accept_loop domains listens pipes maxc in
    r
  with exn ->
    Log.f (Sch 1) (fun k -> k "exn in accept_loop %s" (Printexc.to_string exn));
    raise exn

module Client = struct
  type t = client
  let current = global_get_client
  let connected c = c.connected
  let peer c = c.peer
  let start_time c = c.start_time
  let reset_timeout = reset_timeout
  let set_timeout c f = c.timeout <- f
  let is_ssl c = c.ssl <> None

  let read c s o l = c.read s o l
  let write c s o l = c.write s o l
  let sendfile c s o l = c.sendfile s o l
  let flush c = c.flush ()

  (* All close above where because of error or socket closed on client side.
     close in Server may be because there is no keep alive and the server close,
     so we flush before closing to handle the (very rare) ssl_flush exception
     above. This is only possible to close the current client! *)
  let immediate_close client =
    let dinfo = global_get_dinfo() in
    close ~dinfo ~client ClosedByHandler

  let close cl = cl.timeout_ref <- 0.0

  type at_close = (unit -> unit) LL.cell
  let at_close = at_close
  let remove_at_close = remove_at_close
end
