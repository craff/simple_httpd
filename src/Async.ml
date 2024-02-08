open Effect
open Effect.Deep
module LL = Util.LinkedList

type status = {
    nb_connections : int Atomic.t array;
    domain_ids : Domain.id array;
  }

let max_domain = 16

let new_id =
  let c = ref 0 in
  fun () -> let x = !c in c := x + 1; x

exception NoRead
exception NoWrite
exception EndHandling
exception ClosedByHandler
exception TimeOut

(** Generic type for continuation: used only to discontinue in
    cas of TimeOut *)
type any_continuation =
    N : any_continuation
  | C : ('a,unit) continuation -> any_continuation

type client =
  { id : int
  ; mutable connected : bool
  ; sock : Unix.file_descr
  ; peer : string
  ; accept_by : int (* index of the socket that accepted the connection in the
                       listens table *)
  ; mutable ssl : Ssl.socket option
  ; mutable session : session option
  ; mutable acont : any_continuation
  ; mutable start_time : float (* last time request started *)
  ; mutable timeout_ref : float
  ; mutable locks : mutex list
  ; buf : Buffer.t (* used to parse headers *)
  ; mutable last_seen_cell : client LL.cell
  }

and mutex_state = Unlocked | Locked of client | Deleted

and mutex =
  { mutable eventfd : Unix.file_descr
  ; mutable owner   : mutex_state
  ; waiting : bool array (* which domain has added the mutex to epoll, the
                            first time it had a client blocked while trying to
                            lock *)
  }

and session_info =
  { addr : string
  ; key : string
  ; life_time : float
  ; clients : client list Atomic.t
  ; data : Util.data Atomic.t
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
      acont = N;
      id = -1;
      buf = Buffer.create 16;
      start_time = 0.0;
      timeout_ref = 0.0;
      locks = [];
      accept_by = 0;
      last_seen_cell = LL.fake_cell
    }

let set_session ?session client =
  client.session <- session

type _ Effect.t +=
   | Io  : { sock: Unix.file_descr; fn: (unit -> int) }
           -> int Effect.t
   | Yield : unit Effect.t
   | Sleep : float -> unit Effect.t
   | Lock  : mutex * (mutex -> unit) -> unit Effect.t

type 'a pending =
  { fn : unit -> 'a
  ; cont : ('a, unit) continuation
  }

let apply c f1 f2 =
  match c.ssl with None -> f1 c.sock
                 | Some s -> f2 s

let now = Unix.gettimeofday

let sleep : float -> unit = fun t ->
  let t = now () +. t in
  perform (Sleep t)

module IoTmp = struct

  type t = { sock : Unix.file_descr
           ; waiting : bool array
           (* which domain has added the socket to epoll,
              the first time it had a client blocked while trying to lock *) }
end

type socket_type =
  | Io     (* Io socket *)
  | Client (* main socket serving the http connexion *)
  | Pipe   (* pipe to receive the new connection *)
  | Lock   (* mutex *)

let printexn e =
  match e with
  | e -> (Printexc.to_string e)

type pending_status =
  |  NoEvent
  | Wait : 'a pending -> pending_status
  | TooSoon of Unix.error option

type socket_info =
  { ty : socket_type
  ; client : client
  ; mutable pd : pending_status
  }

type domain_info =
  { mutable cur_client : client (* the client currently running *)
  ; pendings : (Unix.file_descr, socket_info) Hashtbl.t
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
  }

let fake_domain_info =
  { cur_client = fake_client
  ; pendings = Hashtbl.create 16
  ; poll_list = Polly.create ()
  ; bytes = Bytes.create 0
  ; last_seen = LL.create ()
  }

let all_domain_info = Array.make max_domain fake_domain_info

let global_get_client () =
  let id = Domain.self () in
  all_domain_info.((id :> int)).cur_client

module Log = struct
  type log_lvl =
    | Req of int
    | Sch of int
    | Exc of int
    | Aut of int

  type log_status =
    { mutable requests : int
    ; mutable scheduler : int
    ; mutable exceptions : int
    ; mutable authentications : int }

  let log_status =
    { requests = 1
    ; scheduler = 0
    ; exceptions = 1
    ; authentications = 1}

  let set_log_requests        n = log_status.requests   <- n
  let set_log_scheduler       n = log_status.scheduler  <- n
  let set_log_exceptions      n = log_status.exceptions <- n
  let set_log_authentications n = log_status.authentications <- n

  let do_log = function
    | Req n -> n < log_status.requests
    | Sch n -> n < log_status.scheduler
    | Exc n -> n < log_status.exceptions
    | Aut n -> n < log_status.authentications

  let str_log = function
    | Req n -> "Req", n
    | Sch n -> "Sch", n
    | Exc n -> "Exc", n
    | Aut n -> "Aut", n

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
    let size = (Unix.fstat fd).st_size in
    if size <= 0 then
      Printf.fprintf ch "%.6f %2d %10d Log0: log created\n%!"
        (Unix.gettimeofday ()) i (-1);
    ch

  let get_log id =
    let ch = !log_files.(id) in
    (* reopen log file it no link: logrotate might delete it*)
    if Unix.(fstat (Unix.descr_of_out_channel ch)).st_nlink < 1 then
      begin
        (try close_out ch with _ -> ());
        let ch = open_log id in
        !log_files.(id) <- ch;
        ch
      end
    else ch

  let set_log_folder ?(basename="log") ?(perm=0o700) folder nb_dom =
    log_perm := perm;
    log_basename := basename;
    log_folder := folder;
    if not (Sys.file_exists folder) then Sys.mkdir folder perm;
    if not (Sys.is_directory folder) then invalid_arg "set_log_folder";
    try
      let a = Array.init nb_dom (fun i -> open_log i) in
      log_files := a
    with e -> failwith ("set_log_folder: " ^ Printexc.to_string e)

  let f ty k =
    if do_log ty then (
      k (fun fmt->
          let id = Domain.((self() :> int)) in
          let cl = global_get_client () in
          let ch =
            if id < Array.length !log_files then get_log id else stdout
          in
          let log,lvl = str_log ty in
          Printf.fprintf ch "%.6f %2d %10d %s%d: "
            (Unix.gettimeofday ()) id cl.id log lvl;
          Printf.kfprintf (fun oc -> Printf.fprintf oc "\n%!") ch fmt
        )
    )

  let fname id =
    let _ = get_log id in
    fname id

  let _ = Address.forward_log := f (Exc 0)
end

let yield () = perform Yield
(*
let schedule () =
  let id = Domain.self () in
  let time = all_domain_info.((id :> int)).schedule in
  let now = now () in
  if now >= time then yield ()
 *)
module Mutex : sig
    type t = mutex
    val create : unit -> t
    val unlock : t -> unit
    val try_lock : t -> bool
    val lock : t -> unit
    val delete : t -> unit
  end = struct
  external raw_eventfd  : int -> int -> Unix.file_descr = "caml_eventfd"
  (*external raw_efd_cloexec   : unit -> int = "caml_efd_cloexec"*)
  external raw_efd_nonblock  : unit -> int = "caml_efd_nonblock"

  let flags = raw_efd_nonblock()

  type t = mutex

  let delete r =
    match r.owner with
    | Deleted -> ()
    | Locked _ ->
       Log.(f (Exc 0)) (fun k -> k "Mutex collected before unlock!");
    | Unlocked -> r.owner <- Deleted;
                  try Unix.close r.eventfd with _ -> ()

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
      | Locked cl ->
         if cl != global_get_client () then
           failwith "unlock by a client that did not lock the mutex";
         cl.locks <- List.filter (fun x -> x != lk) cl.locks
      | Unlocked ->
         failwith "unlock a not locked mutex"
      | Deleted ->
         failwith "unlock a deleted mutex"
    end;
    lk.owner <- Unlocked;
    assert (Util.single_write lk.eventfd one 0 8 = 8)

  let try_lock lk =
    let buf = Bytes.create 8 in
    try assert(Util.read lk.eventfd buf 0 8 = 8);
        let cl = global_get_client () in
        lk.owner <- Locked cl;
        cl.locks <- lk :: cl.locks;
        true
    with Unix.(Unix_error((EAGAIN | EWOULDBLOCK), _, _)) -> false

  let rec lock : t -> unit = fun lk ->
    if not (try_lock lk) then perform (Lock (lk, lock))
end

module Ssl = struct include Ssl include Ssl.Runtime_lock end

let rec read c s o l =
  try
    apply c Unix.read Ssl.read s o l
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_))
     | Ssl.(Read_error(Error_want_read|Error_want_write
                        |Error_want_connect|Error_want_accept|Error_zero_return)) ->        perform_read c s o l

and perform_read c s o l =
  perform (Io {sock = c.sock; fn = (fun () -> read c s o l) })

let rec write c s o l =
  try
    apply c Unix.single_write Ssl.write s o l
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_))
    | Ssl.(Write_error(Error_want_read|Error_want_write
                       |Error_want_connect|Error_want_accept|Error_zero_return)) ->
     perform_write c s o l

and perform_write c s o l =
  perform (Io {sock = c.sock; fn = (fun () -> write c s o l) })

(* Note: sendfile together with SSL, not really efficient *)
let ssl_sendfile c fd o l =
  let buf = all_domain_info.((Domain.self () :> int)).bytes in
  let len = Bytes.length buf in
  let _ = Unix.(lseek fd o SEEK_SET) in
  let w = Unix.read fd buf 0 (min l len) in
  Ssl.write c buf 0 w

let rec sendfile c fd o l =
  try
    apply c Util.sendfile ssl_sendfile fd o l
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_))
     | Ssl.(Write_error(Error_want_read|Error_want_write
                        |Error_want_connect|Error_want_accept|Error_zero_return)) ->
          perform_sendfile c fd o l

and perform_sendfile c fd o l =
  perform (Io {sock = c.sock; fn = (fun () -> sendfile c fd o l) })

let schedule_io sock fn =
  perform (Io {sock; fn })

let cur_client () =
  let i = all_domain_info.((Domain.self () :> int)) in
  i.cur_client

let register_starttime cl =
  let r = now () in
  cl.start_time <- r;
  cl.timeout_ref <- r;
  let ll = all_domain_info.((Domain.self () :> int)).last_seen in
  Util.LinkedList.move_first cl.last_seen_cell ll;
  r

let reset_timeout cl =
  let r = now () in
  cl.timeout_ref <- r;
  let ll = all_domain_info.((Domain.self () :> int)).last_seen in
  Util.LinkedList.move_first cl.last_seen_cell ll

module Io = struct
  include IoTmp

  let close (s:t) =
    (try Unix.(shutdown s.sock SHUTDOWN_ALL); with _ -> ());
    (try Unix.close s.sock with _ -> ())

  let register (r : t) =
    let i = (Domain.self () :> int) in
    let info = all_domain_info.(i) in
    if not r.waiting.(i) then begin
        r.waiting.(i) <- true;
        Polly.(add info.poll_list r.sock Events.(inp lor out lor et lor rdhup));
      end;
    let c = cur_client () in
    Hashtbl.replace info.pendings r.sock { ty = Io; client = c; pd = NoEvent }

  let create sock =
    let r = { sock
            ; waiting = Array.make max_domain false }
    in
    Gc.finalise close r;
    r

  let rec read (io:t) s o l =
  try
    Util.read io.sock  s o l
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_)) ->
    register io;
    schedule_io io.sock (fun () -> read io s o l)

  let rec write (io:t) s o l =
  try
    Util.single_write io.sock s o l
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_)) ->
        register io;
        schedule_io io.sock (fun () -> write io s o l)

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
    Unix.set_nonblock sock;
    Unix.(setsockopt_optint sock SO_LINGER None);
    Unix.(setsockopt sock SO_REUSEADDR reuse);
    let inet_addr = Unix.inet_addr_of_string addr in
    Unix.bind sock (Unix.ADDR_INET (inet_addr, port));
    Unix.listen sock maxc;
    sock
  with e -> (try Unix.close sock; with _ -> ()); raise e

type pollResult =
  | Accept of (int * Unix.file_descr * Address.t)
  | Action : 'a pending * socket_info * Unix.error option -> pollResult
  | Yield of ((unit,unit) continuation * client * float)
  | Wait

let loop id st listens pipe timeout handler () =
  let did = Domain.self () in
  st.domain_ids.(id) <- did;

  let poll_list = Polly.create () in
  Polly.(add poll_list pipe Events.(inp lor et));
  (* size for two ints *)
  let pipe_buf = Bytes.create 8 in
  (* table of all sockets *)
  let pendings : (Unix.file_descr, socket_info) Hashtbl.t
    = Hashtbl.create 16384
  in
  let pipe_info = { ty = Pipe; client = fake_client; pd = NoEvent } in
  Hashtbl.add pendings pipe pipe_info;

  let bytes = Bytes.create (16 * 4_096) in
  let last_seen = LL.create () in
  let dinfo = { cur_client = fake_client; pendings ; poll_list; bytes; last_seen} in
  let get_client () = dinfo.cur_client in
  all_domain_info.((did :> int)) <- dinfo;


  (* Queue for ready sockets *)
  let ready = Queue.create () in
  let add_ready e = Queue.add e ready in
  add_ready Wait; (* Invariant: queue as always exactly one Wait
                     and is never empty, except during poll. *)

  (* Managment of sleep *)
  let sleeps = ref [] in
  (* O(N) when N is the current number of sleep. Could use Map ?*)
  let add_sleep t cont =
    let cl = get_client () in
    cl.acont <- C cont;
    let rec fn acc = function
      | [] -> List.rev_append acc [(t,cont,cl)]
      | (t',_,_)::_ as l when t < t' -> List.rev_append acc ((t,cont,cl)::l)
      | c::l -> fn (c::acc) l
    in
    sleeps := fn [] !sleeps
  in
  (* amortized O(1) *)
  let get_sleep now =
    let rec fn l =
      match l with
      | (t,cont,cl)::l when t <= now ->
         if cl.connected then
           begin
             Log.f (Sch 2) (fun k -> k "[%d] end sleep" cl.id);
             add_ready (Yield(cont,cl,now));
           end;
         fn l
      | l -> l
    in
    sleeps := fn !sleeps
  in

  (* Managment of lock *)
  (* O(1) *)
  let add_lock : Mutex.t -> (Mutex.t -> unit) -> (unit, unit) continuation -> unit =
    fun lk fn cont ->
    let cl = get_client () in
    cl.acont <- C cont;
    let fn () = fn lk in
    let info = { ty = Lock ; client = cl
               ; pd = Wait { fn; cont }
               }
    in
    (* may be another client is already waiting *)
    if not lk.waiting.((did :> int)) then
      begin
        Polly.(add poll_list lk.eventfd Events.(inp lor et));
        lk.waiting.((did :> int)) <- true
      end;

    Hashtbl.add pendings lk.eventfd info;

  in

  let find s = Hashtbl.find_all pendings s in

  let close ?client exn =
    let c = match client with None -> get_client () | Some c -> c in
    assert c.connected;
    LL.remove_cell c.last_seen_cell last_seen;
    begin
      let fn s =
        (try Ssl.shutdown s with _ -> ());
        (try Unix.close (Ssl.file_descr_of_socket s) with _ -> ())
      in
      let gn s =
        (try Unix.shutdown s SHUTDOWN_ALL with _ -> ());
        (try Unix.close s with _ -> ())
      in
      try apply c gn fn with Unix.Unix_error _ -> ()
    end;
    Hashtbl.remove pendings c.sock;
    Atomic.decr st.nb_connections.(id);

    begin
      match c.session with
      | None -> ()
      | Some sess ->
         let sess = LL.get sess in
         let fn clients = List.filter (fun c' -> c != c') clients in
         Util.update_atomic sess.clients fn;
    end;
    begin
      List.iter Mutex.unlock c.locks;
      c.locks <- []
    end;
    Log.(f (Exc 1))
      (fun k -> k "[%d] closing because exception: %s. connected: %b (%d)"
                  c.id (printexn exn) c.connected
                  (Atomic.get st.nb_connections.(id)));
    c.connected <- false
  in

  let rec remove_timeout t0 =
    try
      let cell = LL.tail last_seen in
      let client = LL.get cell in
      if client.timeout_ref < t0 then
        begin
          Log.(f (Exc 1)) (fun k -> k "[%d] timout: closing" client.id);
          close ~client TimeOut;
          remove_timeout t0
        end
    with Not_found -> ()
  in

  let rec poll () =
    let now = now () in
    try
      (* O(n) when n is the number of waiting lock *)
      get_sleep now;
      remove_timeout (now -. timeout);
      let select_timeout =
        match Queue.is_empty ready, !sleeps with
        | false, _ -> 0.0
        | _, (t,_,_)::_ -> min timeout (t -. now)
        | _ -> timeout
      in
      let select_timeout = int_of_float (1e3 *. select_timeout) in
      let fn _ sock evt =
        let fn info =
          (match info with
           | { ty = (Lock | Io); _ } ->
              Hashtbl.remove pendings sock;
           | _ -> ());
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
                   Log.f (Req 2) (fun k -> k "received accepted socket");
                   add_ready (Accept (index, sock, l))
                 done
               with Unix.Unix_error((EAGAIN|EWOULDBLOCK),_,_) -> ()
                  | e -> Log.f (Exc 0) (fun k -> k "unexpected accept recv: %s"
                                                   (Printexc.to_string e))
             end
          | { pd = NoEvent ; _ } as r ->
             let e =
               if Polly.Events.((err lor hup lor rdhup) land evt <> empty) then
                 Unix.(getsockopt_error sock)
               else None
             in
             r.pd <- TooSoon e
          | { pd = Wait a ; _ } as p ->
             let e =
               if Polly.Events.((err lor hup lor rdhup) land evt <> empty) then
                 Unix.(getsockopt_error sock)
               else None
             in
             add_ready (Action(a,p,e));
             p.pd <- NoEvent;
          | { pd = TooSoon b ; _ } as p ->
             let e =
               if Polly.Events.((err lor hup lor rdhup) land evt <> empty) then
                 match Unix.(getsockopt_error sock) with None -> b | e -> e
               else b
             in
             p.pd <- TooSoon e
        in
        List.iter fn (find sock)
      in
      try
        ignore (Polly.wait poll_list 1000 select_timeout fn);
        add_ready Wait
      with e -> add_ready Wait; raise e
    with
    | exn -> Log.f (Exc 0) (fun k -> k "unexpected poll: %s\n%!" (printexn exn));
             (*check now;*) poll () (* FIXME: which exception *)
  in
  let step v =
    try
      match v with
      | Wait ->
         poll ()
      | Accept (index, sock, linfo) ->
         let peer = Util.addr_of_sock sock in
         let now = now () in
         let client = { sock; ssl = None; id = new_id ()
                      ; peer
                      ; connected = true; session = None
                      ; start_time = now; timeout_ref = now
                      ; locks = []
                      ; acont = N; buf = Buffer.create 4_096
                      ; accept_by = index; last_seen_cell = LL.fake_cell
                      }
         in
         client.last_seen_cell <- LL.add_first client dinfo.last_seen;
         dinfo.cur_client <- client;
         let info = { ty = Client; client; pd = NoEvent } in
         Log.f (Req 1)
           (fun k -> k "[%d] accept connection from %s" client.id client.peer);
         Unix.set_nonblock sock;
         Unix.(setsockopt_float sock SO_RCVTIMEO timeout);
         Unix.(setsockopt_float sock SO_SNDTIMEO timeout);
         Unix.(setsockopt sock TCP_NODELAY true); (* not clearly usefull *)
         Hashtbl.add pendings sock info;
         Polly.(add poll_list sock Events.(inp lor out lor et lor rdhup));
         begin
           match linfo.ssl with
           | Some ctx ->
              let chan = Ssl.embed_socket sock (Atomic.get ctx) in
              let rec fn () =
                try
                  Ssl.accept chan; 1
                with
                | Ssl.(Accept_error(Error_want_read|Error_want_write
                                   |Error_want_connect|Error_want_accept|Error_zero_return)) ->
                   perform (Io {sock; fn })
              in
              ignore (fn ());
              client.ssl <- Some chan;
              Log.f (Req 2) (fun k -> k "[%d] ssl connection established" client.id);
           | None -> ()
         end;
         handler client; close EndHandling
      | Action ({ fn; cont; _ }, p, e) ->
         p.pd <- NoEvent;
         let cl = p.client in
         if cl.connected then
           begin
             dinfo.cur_client <-cl;
             cl.acont <- N;
             match e with
             | Some e ->
                let msg = Unix.error_message e in
                Log.f (Sch 0) (fun k ->
                    k "[%d] discontinue io %s" cl.id msg);
                discontinue cont (Unix.Unix_error(e, msg, "error_in_poll"))
             | None ->
                Log.f (Sch 0) (fun k -> k "[%d] continue io" cl.id);
                let n = fn () in
                continue cont n;
           end
      | Yield(cont,cl,_) ->
         if cl.connected then
           begin
             dinfo.cur_client <- cl;
             cl.acont <- N;
             Log.f (Sch 0) (fun k -> k "[%d] continue yield" cl.id);
             continue cont ();
           end
    with e -> (try close e
               with e -> Log.f (Exc 1) (fun k -> k "close: %s"
                                                (Printexc.to_string e)))

  in
  let step_handler v =
    try_with step v
      { effc = (fun (type c) (eff: c Effect.t) ->
        match eff with
        | Yield ->
           Some (fun (cont : (c,_) continuation) ->
               let c = get_client () in
               c.acont <- C cont;
               add_ready (Yield(cont, c, now ())))
        | Sleep(t) ->
           Some (fun (cont : (c,_) continuation) ->
               add_sleep t cont)
        | Lock(lk, fn) ->
           Some (fun (cont : (c,_) continuation) ->
               add_lock lk fn cont)
        | Io {sock; fn; _} ->
           Some (fun (cont : (c,_) continuation) ->
               (get_client ()).acont <- C cont;
               let infos = find sock in
               let fn info =
                 match info.pd with
                 | NoEvent ->
                    info.pd <- Wait { fn; cont }
                 | TooSoon e ->
                    add_ready (Action({ fn; cont }, info, e))
                 | Wait _ -> assert false
               in
               List.iter fn infos)
        | _ -> None
    )}
  in
  while true do
    step_handler (Queue.take ready)
  done

let add_close, close_all =
  let to_close = ref [] in
  let add_close s = to_close := s :: !to_close in
  let close_all s =
    Printf.eprintf "Exit on signal: %d\n%!" s;
    List.iter Unix.close !to_close;
    exit 1
  in
  (add_close, close_all)

let _ = Sys.(set_signal sigint  (Signal_handle close_all))
let _ = Sys.(set_signal sigterm (Signal_handle close_all))
let _ = Sys.(set_signal sigquit (Signal_handle close_all))
let _ = Sys.(set_signal sigabrt (Signal_handle close_all))


let accept_loop status listens pipes maxc =
  let exception Full in
  let poll_list = Polly.create () in
  let nb = Array.length pipes in
  let tbl = Hashtbl.create (nb * 4) in
  let pipe_buf = Bytes.create 8 in
  Array.iteri (fun i (s,_) ->
      add_close s;
      Hashtbl.add tbl s i;
      Polly.(add poll_list s Events.(inp lor et))) listens;

  let get_best () =
    let index = ref 0 in
    let c = ref (Atomic.get status.nb_connections.(0)) in
    let t = ref !c in
    for i = 1 to nb - 1 do
      let c' = Atomic.get status.nb_connections.(i) in
      t := !t + c';
      if c' < !c then (index := i; c := c')
    done;
    if !t >= maxc then raise Full;
    (!index, pipes.(!index))
  in
  let treat _ sock _ =
    let continue = ref true in
    while !continue do
      let to_close = ref None in
      try
        let index = try Hashtbl.find tbl sock with Not_found -> assert false in
        let (did, pipe) = get_best () in
        let (lsock, _) = Unix.accept sock in
        to_close := Some lsock;
        Bytes.set_int32_ne pipe_buf 0 (Int32.of_int (Util.file_descr_to_int lsock));
        Bytes.set_int32_ne pipe_buf 4 (Int32.of_int index);
        assert(Util.single_write pipe pipe_buf 0 8 = 8);
        Log.f (Req 2) (fun k -> k "send accepted socket to domain %d" did);
        Atomic.incr status.nb_connections.(did);
      with
      | Full ->
         Log.f (Exc 0) (fun k -> k "handler: reject too many clients");
         let (lsock, _) = Unix.accept sock in
         Unix.close lsock
      | Unix.Unix_error((EAGAIN|EWOULDBLOCK),_,_) -> continue := false
      | exn ->
         begin
           match !to_close with
           | None -> ()
           | Some s -> try Unix.close s with Unix.Unix_error _ -> ()
         end;
         Log.f (Exc 0) (fun k -> k "accept send: %s" (printexn exn))
    done
  in
  let nb_socks = Array.length listens in
  while true do
    try ignore (Polly.wait poll_list nb_socks 60_000_000 treat)
    with
    | Unix.Unix_error((EAGAIN|EWOULDBLOCK),_,_) -> ()
    | exn ->
       (* normal if client close connection brutally? *)
       Log.f (Exc 1) (fun k -> k "epoll_wait: %s" (printexn exn))
  done

let run ~nb_threads ~listens ~maxc ~timeout ~status handler =
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
    Domain.spawn (loop id status listens_r r timeout handler)
  in
  let pipes = Array.map snd pipes in
  let r = Array.init nb_threads fn in
  let _ = accept_loop status listens pipes maxc in
  r

let rec ssl_flush s =
  try ignore (Ssl.flush s); 1
  with Ssl.Flush_error(true) ->
    schedule_io (Ssl.file_descr_of_socket s) (fun () -> ssl_flush s)

module Client = struct
  type t = client
  let connected c = c.connected
  let peer c = c.peer
  let start_time c = c.start_time
  let is_ssl c = c.ssl <> None
  let ssl_flush c = apply c (fun _ -> ()) (fun s -> ignore (ssl_flush s))

  (* All close above where because of error or socket closed on client side.
     close in Server may be because there is no keep alive and the server close,
     so we flush before closing to handle the (very rare) ssl_flush exception
     above *)
  let close c = ssl_flush c; raise ClosedByHandler
end
