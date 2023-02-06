open Effect
open Effect.Deep
open Domain

module U = Simple_httpd_util

type status = {
    nb_connections : int Atomic.t array
  }

let max_domain = 16

type session_data = ..
type session_data += NoData

module MutexTmp : sig
    type t = Unix.file_descr
    val create : unit -> t
    val unlock : t -> unit
    val try_lock : t -> bool
  end = struct
  external raw_eventfd  : int -> int -> Unix.file_descr = "caml_eventfd"
  (*external raw_efd_cloexec   : unit -> int = "caml_efd_cloexec"*)
  external raw_efd_nonblock  : unit -> int = "caml_efd_nonblock"
  external raw_efd_semaphore : unit -> int = "caml_efd_semaphore"

  let flags = raw_efd_nonblock() land raw_efd_semaphore()

  type t = Unix.file_descr

  let create () = raw_eventfd 1 flags

  let one = let r = Bytes.create 8 in Bytes.set_int64_ne r 0 1L; r

  let unlock lk =
    assert (Simple_httpd_util.single_write lk one 0 8 = 8)

  let try_lock lk =
    let buf = Bytes.create 8 in
    try assert(Simple_httpd_util.read lk buf 0 8 = 8); true
    with Unix.(Unix_error((EAGAIN | EWOULDBLOCK), _, _)) -> false
end
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

type client = {
    id : int;
    mutable connected : bool;
    sock : Unix.file_descr;
    mutable ssl : Ssl.socket option;
    mutable session : session option;
    mutable acont : any_continuation;
    mutable start_time : float; (* last time request started *)
    mutable locks : MutexTmp.t list;
    buf : Buffer.t; (* used to parse headers *)
  }

and session =
  { addr : string
  ; key : string
  ; mutex : MutexTmp.t
  ; mutable clients : client list
  ; mutable data : session_data
  ; mutable cleanup : session_data -> unit
  ; mutable cookies : (string * string) list
  }

let fake_client =
    { sock = Unix.stdout;
      ssl = None;
      connected = false;
      session = None;
      acont = N;
      id = -1;
      buf = Buffer.create 16;
      start_time = 0.0;
      locks = [];
    }

type _ Effect.t +=
   | Io  : { sock: Unix.file_descr; fn: (unit -> int) }
           -> int Effect.t
   | Yield : unit Effect.t
   | Sleep : float -> unit Effect.t
   | Lock  : MutexTmp.t * (MutexTmp.t -> unit) -> unit Effect.t

type 'a pending =
  { fn : unit -> 'a
  ; cont : ('a, unit) continuation
  ; mutable arrival_time : float (* time it started to be pending *)
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
           ; unregister : unit -> unit
           ; client : client }
end

type socket_type =
  | Io of IoTmp.t
  | Client of client
  | Pipe
  | Lock of client

exception SockError of socket_type * exn

let printexn e =
  match e with
  | SockError (_, e) -> Printf.sprintf "SockError(%s)" (Printexc.to_string e)
  | e -> (Printexc.to_string e)

let clientError c exn = raise (SockError(Client c,exn))
let ioError s exn = raise(SockError(Io s,exn))

type pending_status =
  NoEvent | Wait : 'a pending -> pending_status | TooSoon

type socket_info =
  { ty : socket_type
  ; mutable pd : pending_status
  }

let socket_client s = match s.ty with
  | Client c -> c
  | Io s -> s.client
  | Lock c -> c
  | Pipe -> assert false

type domain_info =
  { (*mutable schedule : float
  ; *)cur_client : client option ref
  ; pendings : (Unix.file_descr, socket_info) Hashtbl.t
  ; poll_list : Polly.t
  }

let fake_domain_info =
  { (*schedule = 0.0
  ; *)cur_client = ref None
  ; pendings = Hashtbl.create 16
  ; poll_list = Polly.create ()
  }

let all_domain_info = Array.make max_domain fake_domain_info

let string_status st =
  let b = Buffer.create 128 in
  Printf.bprintf b "[%t] [%t]"
    (fun b ->
      Array.iteri (fun i a -> Printf.bprintf b "%s%d"
                                (if i = 0 then "" else ", ")
                                (Atomic.get a)) st.nb_connections)
    (fun b ->
      Array.iteri (fun i a -> Printf.bprintf b "%s%d"
                                (if i = 0 then "" else ", ")
                                (Hashtbl.length a.pendings)) all_domain_info);
  Buffer.contents b

let print_status ch st =
  output_string ch (string_status st)

let yield () = perform Yield
(*
let schedule () =
  let id = Domain.self () in
  let time = all_domain_info.((id :> int)).schedule in
  let now = now () in
  if now >= time then yield ()
 *)
module Mutex = struct
  include MutexTmp

  let rec lock : t -> unit = fun lk ->
    if not (try_lock lk) then perform (Lock (lk, lock))
(*
  let wait_bool : bool Atomic.t -> unit = fun b ->
    if not (Atomic.get b) then perform (Lock (b, Atomic.get))
 *)
end

let rec fread c s o l =
  try
    let n = apply c U.read Ssl.read s o l in
    if n = 0 then clientError c NoRead; n
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_))
     | Ssl.(Read_error(Error_want_read|Error_want_accept|
                       Error_want_connect|Error_want_write|Error_zero_return)) ->
        perform_read c s o l
     | exn ->
        clientError c exn

and read c s o l =
  (*schedule ();*) fread c s o l

and perform_read c s o l =
  perform (Io {sock = c.sock; fn = (fun () -> fread c s o l) })

let rec fwrite c s o l =
  try
    let n = apply c U.single_write Ssl.write s o l in
    if n = 0 then clientError c NoWrite; n
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_))
     | Ssl.(Write_error(Error_want_write|Error_want_read|
                        Error_want_accept|Error_want_connect|Error_zero_return)) ->
        perform_write c s o l
     | exn -> clientError c exn

and write c s o l =
  (*schedule ();*) fwrite c s o l

and perform_write c s o l =
  perform (Io {sock = c.sock; fn = (fun () -> fwrite c s o l) })

let schedule_io sock fn =
  perform (Io {sock; fn })

let cur_client () =
  let i = all_domain_info.((Domain.self () :> int)) in
  match !(i.cur_client) with Some c -> c | None -> assert false

let register_starttime cl =
  cl.start_time <- now ()

module type Io = sig
  type t

  val create : Unix.file_descr -> t
  val close : t -> unit
  val read : t -> Bytes.t -> int -> int -> int
  val write : t -> Bytes.t -> int -> int -> int
end

module Io = struct
  include IoTmp

  let unregister s =
    let i = all_domain_info.((Domain.self () :> int)) in
    Hashtbl.remove i.pendings s;
    try Polly.(del i.poll_list s)
    with e -> U.debug ~lvl:1 (fun k -> k "UNEXPECTED EXCEPTION IN EPOLL_DEL: %s"
                                         (printexn e))
  let close (s:t) =
    s.unregister ();
    Unix.close s.sock

  let register s (r : t) =
    let i = all_domain_info.((Domain.self () :> int)) in
    Hashtbl.add i.pendings s { ty = Io r; pd = NoEvent };
    try Polly.(add i.poll_list s Events.(inp lor out lor et))
    with e -> U.debug ~lvl:1 (fun k -> k "UNEXPECTED EXCEPTION IN EPOLL_ADD: %s"
                                         (printexn e));
              raise e

  let create sock =
    let r = { sock
            ; unregister = (fun () -> unregister sock)
            ; client = cur_client () }
    in
    register sock r;
    r

  let rec fread (io:t) s o l =
  try
    let n = U.read io.sock  s o l in
    if n = 0 then ioError io NoRead; n
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_)) ->
        schedule_io io.sock (fun () -> fread io s o l)
     | exn -> ioError io exn

  and read sock io o l =
    (*schedule ();*) fread sock io o l

  let rec fwrite (io:t) s o l =
  try
    let n = U.single_write io.sock s o l in
    if n = 0 then ioError io NoWrite; n
  with Unix.(Unix_error((EAGAIN|EWOULDBLOCK),_,_)) ->
        schedule_io io.sock (fun () -> fwrite io s o l)
     | exn -> ioError io exn

  and write sock s o l =
    (*schedule ();*) fwrite sock s o l
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
  try
    Unix.set_nonblock sock;
    Unix.setsockopt_optint sock Unix.SO_LINGER None;
    let inet_addr = Unix.inet_addr_of_string addr in
    Unix.bind sock (Unix.ADDR_INET (inet_addr, port));
    Unix.listen sock maxc;
    sock
  with e -> Unix.close sock; raise e

type listenning = {
    addr : string;
    port : int;
    ssl  : Ssl.context option ;
  }

type pollResult =
  | Accept of (Unix.file_descr * listenning)
  | Action : 'a pending * socket_info -> pollResult
  | Yield of ((unit,unit) continuation * client * float)
  | Wait

let loop id st listens pipe _delta timeout handler () =
  let did = Domain.self () in
(*
  let set_schedule delta =
    let now = now () in
    all_domain_info.((did :> int)).schedule <- now +. delta
  in
 *)
  let poll_list = Polly.create () in
  Polly.(add poll_list pipe Events.(inp lor et));
  (* size for two ints *)
  let pipe_buf = Bytes.create 8 in
  (* table of all sockets *)
  let pendings : (Unix.file_descr, socket_info) Hashtbl.t
    = Hashtbl.create 16384
  in
  let pipe_info = { ty = Pipe; pd = NoEvent } in
  Hashtbl.add pendings pipe pipe_info;

  let cur_client = ref None in
  let get_client () =
    match !cur_client with
    | Some c -> c
    | None -> assert false
  in
  all_domain_info.((did :> int)) <-
    { (*schedule = now ();*) cur_client; pendings ; poll_list };

  let unregister s =
    Hashtbl.remove pendings s;
    try Polly.(del poll_list s)
    with e -> U.debug (fun k -> k "Unexpected exception in epoll_del: %s"
                                  (printexn e))
  in

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
             U.debug ~lvl:3 (fun k -> k "[%d] end sleep" cl.id);
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
    cl.locks <- lk :: cl.locks;
    let fn () =
      Hashtbl.remove pendings lk;
      Polly.(del poll_list lk);
      cl.locks <- List.filter (fun x -> x <> lk) cl.locks;
      fn lk
    in
    let info = { ty = Lock cl
               ; pd = Wait { fn; cont; arrival_time = now ()}
               }
    in
    Hashtbl.add pendings lk info;
    Polly.(add poll_list lk Events.(inp lor et));
  in

  let find s =
    try Hashtbl.find pendings s with _ -> assert false
  in

  let close ?client exn =
    let c = match client with None -> get_client () | Some c -> c in
    U.debug ~lvl:1 (fun k -> k "closing because exception: %s. connected: %b (%d)"
                               (printexn exn) c.connected
                               (Atomic.get st.nb_connections.(id)));
    assert c.connected;
    unregister c.sock;
    Atomic.decr st.nb_connections.(id);
    begin
      let fn s =
        (try Ssl.shutdown s with Unix.Unix_error _ -> ());
        Unix.close (Ssl.file_descr_of_socket s)
      in
      try apply c Unix.close fn with Unix.Unix_error _ -> ()
    end;
    begin
      match c.session with
      | None -> ()
      | Some sess ->
         Mutex.lock sess.mutex;
         sess.clients <- List.filter (fun c' -> c != c') sess.clients;
         Mutex.unlock sess.mutex;
    end;
    begin
      let fn lk =
        Mutex.unlock lk;
        Hashtbl.remove pendings lk;
        try Polly.(del poll_list lk) with Unix.Unix_error _ -> ();
      in
      List.iter fn c.locks;
      c.locks <- []
    end;
    c.connected <- false
  in

  let rec poll () =
    let now = now () in
    try
      (* O(n) when n is the number of waiting lock *)
      get_sleep now;
      let select_timeout =
        match Queue.is_empty ready, !sleeps with
        | false, _ -> 0.0
        | _, (t,_,_)::_ -> (t -. now)
        | _ -> -1.0
      in
      let select_timeout = int_of_float (1e3 *. select_timeout) in
      let fn _ sock evt =
        let info = find sock in
        if (Polly.Events.((err lor hup) land evt <> empty)) then
          close ~client:(socket_client info) Exit
        else match info with
        | { ty = Pipe; _ } ->
           begin
             try
               while true do
                 assert (U.read pipe pipe_buf 0 8 = 8);
                 let sock : Unix.file_descr =
                   Obj.magic (Int32.to_int (Bytes.get_int32_ne pipe_buf 0))
                 in
                 let index = Int32.to_int (Bytes.get_int32_ne pipe_buf 4) in
                 let l = listens.(index) in
                 add_ready (Accept (sock, l))
               done
             with Unix.Unix_error((EAGAIN|EWOULDBLOCK),_,_) -> ()
           end
        | { pd = NoEvent ; _ } as r ->
           r.pd <- TooSoon
        | { pd = Wait a ; _ } as p ->
           add_ready (Action(a,p));
           p.pd <- NoEvent;
        | { pd = TooSoon; _ } -> ()
      in
      ignore (Polly.wait poll_list 1000 select_timeout fn);
      add_ready Wait
    with
    | exn -> U.debug ~lvl:1 (fun k -> k "UNEXPECTED EXCEPTION IN POLL: %s\n%!"
                                        (printexn exn));
             (*check now;*) poll () (* FIXME: which exception *)
  in
  let step v =
    try
      match v with
      | Wait ->
         poll ()
      | Accept (sock, linfo) ->
         cur_client := None;
         (*set_schedule delta;*)
         let client = { sock; ssl = None; id = new_id ();
                        connected = true; session = None;
                        start_time = now (); locks = [];
                        acont = N; buf = Buffer.create 4_096
                      } in
         cur_client := Some client;
         let info = { ty = Client client
                    ; pd = NoEvent
                    }
         in
         U.debug ~lvl:2 (fun k -> k "[%d] accept connection (%a)" client.id print_status st);
         Unix.set_nonblock sock;
         Unix.(setsockopt_float sock SO_RCVTIMEO timeout);
         Unix.(setsockopt_float sock SO_SNDTIMEO timeout);
         Hashtbl.add pendings sock info;
         Polly.(add poll_list sock Events.(inp lor out lor et));
         begin
           match linfo.ssl with
           | Some ctx ->
              let chan = Ssl.embed_socket sock ctx in
              let rec fn () =
                try Ssl.accept chan; 1
                with
                | Ssl.(Accept_error(Error_want_read|Error_want_write
                                   |Error_want_connect|Error_want_accept|Error_zero_return)) ->
                   perform (Io {sock; fn })
              in
              ignore (fn ());
              client.ssl <- Some chan;
              U.debug ~lvl:2 (fun k -> k "[%d] ssl connection established" client.id);
           | None -> ()
         end;
         handler client; close EndHandling
      | Action ({ fn; cont; _ }, p) ->
         p.pd <- NoEvent;
         let cl = socket_client p in
         if cl.connected then
           begin
             cur_client := Some cl;
             cl.acont <- N;
             U.debug ~lvl:3 (fun k -> k "[%d] continue io" cl.id);
             (*set_schedule delta;*)
             let n = fn () in
             continue cont n;
           end
      | Yield(cont,cl,_) ->
         if cl.connected then
           begin
             cur_client := Some cl;
             cl.acont <- N;
             U.debug ~lvl:3 (fun k -> k "[%d] continue yield" cl.id);
             (*set_schedule delta;*)
             continue cont ();
           end
    with e -> close e

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
               let now = now () in
               let info = find sock in
               (get_client ()).acont <- C cont;
               begin
                 match info.pd with
                 | NoEvent ->
                    info.pd <- Wait { fn; cont
                                      ; arrival_time = now}
                 | TooSoon ->
                    add_ready (Action({ fn; cont
                                 ; arrival_time = now}, info))
                 | Wait _ -> assert false
               end)
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
        assert (Obj.is_int (Obj.repr lsock)); (* Fails on windows *)
        Bytes.set_int32_ne pipe_buf 0 (Int32.of_int (Obj.magic (Obj.repr lsock)));
        Bytes.set_int32_ne pipe_buf 4 (Int32.of_int index);
        assert(U.single_write pipe pipe_buf 0 8 = 8);
        Atomic.incr status.nb_connections.(did);
      with
      | Full ->
         U.debug ~lvl:1 (fun k -> k "REJECT: TOO MANY CLIENTS");
         let (lsock, _) = Unix.accept sock in
         Unix.close lsock
      | Unix.Unix_error((EAGAIN|EWOULDBLOCK),_,_) -> continue := false
      | exn ->
         begin
           match !to_close with
           | None -> ()
           | Some s -> try Unix.close s with Unix.Unix_error _ -> ()
         end;
         U.debug ~lvl:1 (fun k -> k "ERROR DURING ACCEPT: %s" (printexn exn))
    done
  in
  let nb_socks = Array.length listens in
  while true do
    try ignore (Polly.wait poll_list nb_socks 60_000_000 treat)
    with
    | Unix.Unix_error((EAGAIN|EWOULDBLOCK),_,_) -> ()
    | exn ->
       U.debug (fun k -> k "ERROR DURING EPOLL_WAIT: %s" (printexn exn))
  done

let run ~nb_threads ~listens ~maxc ~delta ~timeout ~status handler =
  let listens =
    List.map (fun l ->
        let sock = connect l.addr l.port maxc in
        (sock, l)) listens
  in
  let pipes = Array.init nb_threads (fun _ -> Unix.pipe ()) in
  let listens = Array.of_list listens in
  let listens_r = Array.map snd listens in
  let fn id =
    let (r, _) = pipes.(id) in
    Unix.set_nonblock r;
    spawn (loop id status listens_r r delta timeout handler)
  in
  let pipes = Array.map snd pipes in
  let r = Array.init nb_threads fn in
  let _ = accept_loop status listens pipes maxc in
  r

let rec ssl_flush s =
  try ignore (Ssl.flush s); 1
  with Ssl.Flush_error(true) ->
    schedule_io (Ssl.file_descr_of_socket s) (fun () -> ssl_flush s)

let flush c = apply c (fun _ -> ()) (fun s -> ignore (ssl_flush s))

(* All close above where because of error or socket closed on client side.
   close in Simple_httpd_server may be because there is no keep alive and
   the server close, so we flush before closing to handle the (very rare)
   ssl_flush exception above *)
let close c = flush c; raise ClosedByHandler
