open Async

type session = Async.session
type session_data = Async.session_data

module LinkedList = Util.LinkedList

let addr_of_sock sock =
  match Unix.getsockname sock
  with ADDR_UNIX name -> "UNIX:" ^ name
     | ADDR_INET (addr, _) -> Unix.string_of_inet_addr addr

let get_session, delete_session =
  (* table to search session by key *)
  let sessions_tbl = Hashtbl.create 1024 in
  let mutex_tbl = Mutex.create () in
  (* session orderer by expiration data *)
  let sessions_list = LinkedList.create () in
  let mutex_list = Mutex.create () in
  let delete_session sess =
    let session = LinkedList.get sess in
    Mutex.lock mutex_tbl;
    Hashtbl.remove sessions_tbl session.key;
    let fn clients =
      List.iter (fun cl -> Async.set_session cl) clients;
      []
    in
    Util.update_atomic session.clients fn;
    Mutex.unlock mutex_tbl;
    Mutex.lock mutex_list;
    (try LinkedList.remove_cell sess sessions_list;
         Mutex.unlock mutex_list
     with e ->
       Mutex.unlock mutex_list; raise e);
    (*List.iter Async.close session.clients;*)
    session.cleanup (Atomic.get session.data)
  in
  let refresh session =
    let now = Unix.gettimeofday () in
    Mutex.lock mutex_list;
    try
      LinkedList.move_first session sessions_list;
      (LinkedList.get session).last_refresh <- now;
      while  let l = LinkedList.tail sessions_list in
             let sess = LinkedList.get l in
             LinkedList.is_cell l && now -. sess.last_refresh > sess.life_time do
        let l = LinkedList.tail sessions_list in
        LinkedList.remove_cell l sessions_list;
      done;
      Mutex.unlock mutex_list;
    with e ->
      Mutex.unlock mutex_list;
      raise e
  in
  let get_session ?(session_life_time=3600.0) client key cleanup init =
    Mutex.lock mutex_tbl;
    try
      match client.session with
      | Some s ->
         Mutex.unlock mutex_tbl;
         refresh s;
         (s, true)
      | None ->
         let session = match key with
           | None -> None
           | Some key -> Hashtbl.find_opt sessions_tbl key
         in
         match session with
         | Some session ->
            Async.set_session ~session client;
            Mutex.unlock mutex_tbl;
            let sc = LinkedList.get session in
            let fn clients = client :: clients in
            Util.update_atomic sc.clients fn;
            refresh session;
            (session, true)
         | None ->
            Mutex.unlock mutex_tbl;
            let addr = addr_of_sock client.sock in
            let key = Digest.to_hex
                        (Digest.string (addr ^ string_of_int (Random.int 1_000_000_000))) in
            let data = Atomic.make (init ()) in
            let now = Unix.gettimeofday () in
            let session_info =
              { addr; key; clients= Atomic.make [client]
              ; data; cleanup; cookies = []; life_time = session_life_time
              ; last_refresh = now }
            in
            Mutex.lock mutex_list;
            let session = LinkedList.add_first session_info sessions_list in
            Mutex.unlock mutex_list;
            Mutex.lock mutex_tbl;
            Async.set_session ~session client;
            Hashtbl.add sessions_tbl key session;
            Mutex.unlock mutex_tbl;
            (session, false)
    with e ->
      Mutex.unlock mutex_tbl; raise e
  in
  (get_session, delete_session)

let get_session_data (sess : Async.session) =
  Atomic.get (LinkedList.get sess).data

let do_session_data : Async.session ->  (session_data -> 'a * session_data) -> 'a  =
  fun (sess : Async.session) fn ->
    let session = LinkedList.get sess in
    Util.get_update_atomic session.data fn

let set_session_data (sess : Async.session) data =
  let session = LinkedList.get sess in
  Atomic.set session.data data

let set_session_cookie (sess : Async.session) cname value =
  let session = LinkedList.get sess in
  let cs = List.filter (fun (n,_) -> n <> cname) session.cookies in
  session.cookies <- (cname, value) :: cs

let get_session_cookie (sess : Async.session) cname =
  let session = LinkedList.get sess in
  List.assoc_opt cname session.cookies

let mk_cookies (sess : Async.session) c =
  let session = LinkedList.get sess in
  let max_age = Int64.of_float session.life_time in
  let c = Cookies.create ~name:"SESSION_KEY" ~max_age
            ~same_site:`Strict session.key c in
  let c = Cookies.create ~name:"SESSION_ADDR" ~max_age
            ~same_site:`Strict session.addr c in
  let c = List.fold_left (fun c (name, value) ->
               Cookies.create ~name ~max_age
                 ~same_site:`Strict ~http_only:false value c) c session.cookies in
  c

let check
      ?(session_life_time=3600.0)
      ?(init=fun () -> NoData)
      ?(finalise=fun _ -> ())
      ?(check=fun _ -> true)
      ?(error=(400, [])) req =
  let cookies = Request.cookies req in
  let client = Request.client req in
  let key = Option.map Http_cookie.value
              (Request.get_cookie req "SESSION_KEY")
  in
  let (sess, old) = get_session ~session_life_time client key finalise init in
  let session = LinkedList.get sess in
  try
    if not (check sess) then raise Exit;
    let cookies =
      if old then
        begin
          begin
            match (key, Request.get_cookie req "SESSION_ADDR") with
            | (Some key, Some addr) when
                   key = session.key &&
                     Http_cookie.value addr = session.addr -> ()
            | (None, None) when cookies = [] -> ()
            | _ -> raise Exit
          end;
          let addr = addr_of_sock client.sock in
          if addr <> session.addr then raise Exit;
          cookies
        end
      else
        Cookies.delete_all cookies
    in
    let cookies = mk_cookies sess cookies in
    let gn = Response.update_headers
               (fun h -> Headers.set_cookies cookies h) in
    (req, gn)
  with Exit ->
    delete_session sess;
    let cookies = Cookies.delete_all cookies in
    let (code, headers) = error in
    Response.fail_raise ~headers ~cookies ~code "session ends"

exception NoSession

let get_session req =
  let client = Request.client req in
  let key = Option.map Http_cookie.value
              (Request.get_cookie req "SESSION_KEY")
  in
  let init () = raise NoSession in
  let finalise _ = assert false in
  let (sess, _) = get_session client key finalise init in
  sess
