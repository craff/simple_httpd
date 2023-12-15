
type index = int

let unset_index = -1

type t =
  { addr : string
  ; port : int
  ; hosts : string list
  ; ssl  : Ssl.context Atomic.t option
  ; reuse : bool
  ; mutable index : index ref
    (* shared ref for all adresses using the same ip and port  *)
  }

type ssl =
  { protocol : Ssl.protocol
  ; cert  : string
  ; priv : string
  ; mutable addr : t
  ; mutable mtime : float
  }


let all_ssl = Atomic.make []

let add_ssl ssl =
  Util.update_atomic all_ssl (fun old_ssl -> ssl :: old_ssl)

let forward_log
    : (((('a, out_channel, unit, unit) format4 -> 'a) -> unit) -> unit) ref
    = ref (fun _ -> assert false)

let ssl_reload_period = ref (24 * 3600)

let set_ssl_reload_period : int -> unit = (fun n -> ssl_reload_period := n)

let rec renew_ssl () =
  Unix.sleep !ssl_reload_period;
  let renew ssl =
    try
      !forward_log (fun k -> k "try renew ssl certificate %S, %S%s"
                    ssl.cert ssl.priv "");
      let mtime1 = (Unix.stat ssl.priv).st_mtime in
      let mtime2 = (Unix.stat ssl.cert).st_mtime in
      let mtime = max mtime1 mtime2 in
      if mtime > ssl.mtime then
        begin
          ssl.mtime <- mtime;
          let ctx = Ssl.create_context ssl.protocol Ssl.Server_context in
          Ssl.use_certificate ctx ssl.cert ssl.priv;
          match ssl.addr.ssl with
          | None -> assert false
          | Some a -> Atomic.set a ctx
        end;
      !forward_log (fun k -> k "Renewed ssl certificate %S, %S: %s"
                    ssl.cert ssl.priv "OK")
    with e ->
      !forward_log (fun k -> k "failed to renew ssl certificate %S, %S because %s"
                    ssl.cert ssl.priv (Printexc.to_string e))
  in
  List.iter renew (Atomic.get all_ssl);
  renew_ssl ()

type ssl_info =
  { protocol : Ssl.protocol
  ; cert : string
  ; priv : string
  }

let init_ssl = ref false

let dummy =
  { addr = ""; port = 0; hosts = []; ssl = None
    ; reuse = false; index = ref (-1) }

let change_hosts hosts addr = { addr with hosts }
let change_port port addr = { addr with port ; index = ref unset_index }

let make ?(addr="0.0.0.0") ?(port=8080) ?(hosts=[]) ?ssl ?(reuse=true) () =
  let ctx, fill =
    match ssl with
    | None -> (None, fun _ -> ())
    | Some {protocol; cert; priv} ->
       try
         if not !init_ssl then (
           Ssl_threads.init ();
           Ssl.init ();
           ignore (Thread.create renew_ssl ());
           init_ssl := true);
         let mtime1 = (Unix.stat priv).st_mtime in
         let mtime2 = (Unix.stat cert).st_mtime in
         let mtime = max mtime1 mtime2 in
         let ctx = Ssl.create_context protocol Ssl.Server_context in
         let _ = Ssl.set_min_protocol_version ctx protocol in
         let _ = Ssl.set_max_protocol_version ctx Ssl.TLSv1_3 in
         Ssl.use_certificate ctx cert priv;
         let ssl = { mtime; cert; priv; protocol; addr = dummy } in
         add_ssl ssl;
         (Some (Atomic.make ctx), fun addr -> ssl.addr <- addr)
       with e ->
         !forward_log
           (fun k -> k "failed to read ssl certificate %S, %S because %s"
                       cert priv (Printexc.to_string e));
         raise e
  in
  let addr = { addr ; port ; hosts; ssl = ctx; reuse; index = ref unset_index } in
  fill addr; addr

let register fn addrs =
  let a = Array.of_list addrs in
  (a, Array.mapi (fun i x -> x.index := i; fn x) a)

let index addr =
  let res = !(addr.index) in
  if res < 0 then
    invalid_arg "add_route: the server is not listening to that adress";
  res

let set_index_ref addr i = addr.index <- i
