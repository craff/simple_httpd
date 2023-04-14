open Simple_httpd_domain

module S = Simple_httpd_server
module D = Simple_httpd_domain
module LinkedList = Simple_httpd_util.LinkedList

let addr_of_sock sock =
  match Unix.getsockname sock
  with ADDR_UNIX name -> "UNIX:" ^ name
     | ADDR_INET (addr, _) -> Unix.string_of_inet_addr addr

let get_session, delete_session =
  (* table to search session by key *)
  let sessions_tbl = Hashtbl.create 1024 in
  let mutex_tbl = Mutex.create () in
  (* session orderer by expiration data *)
  let sessions_list = Simple_httpd_util.LinkedList.create () in
  let mutex_list = Mutex.create () in
  let delete_session sess =
    let session = LinkedList.get sess in
    Mutex.lock mutex_tbl;
    Hashtbl.remove sessions_tbl session.key;
    Mutex.unlock mutex_tbl;
    Mutex.lock mutex_list;
    (try LinkedList.remove_cell sess sessions_list;
         Mutex.unlock mutex_list
     with e ->
       Mutex.unlock mutex_list; raise e);
    (*List.iter D.close session.clients;*)
    List.iter (fun cl -> cl.session <- None) session.clients;
    session.cleanup session.data;
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
      Mutex.unlock mutex_list
    with e ->
      Mutex.unlock mutex_list; raise e
  in
  let get_session ?(session_life_time=3600.0) client key init =
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
         | Some s ->
            Mutex.unlock mutex_tbl;
            client.session <- Some s;
            let sc = LinkedList.get s in
            sc.clients <- client :: sc.clients;
            refresh s;
            (s, true)
         | None ->
            let addr = addr_of_sock client.sock in
            let key = Digest.to_hex
                        (Digest.string (addr ^ string_of_int (Random.int 1_000_000_000))) in
            let data = init () in
            let cleanup _ = () in
            let now = Unix.gettimeofday () in
            let session_info =
              { addr; key; clients=[client]; mutex = Mutex.create ()
              ; data; cleanup; cookies = []; life_time = session_life_time
              ; last_refresh = now }
            in
            Mutex.unlock mutex_tbl;
            Mutex.lock mutex_list;
            let session = LinkedList.add_first session_info sessions_list in
            Mutex.unlock mutex_list;
            client.session <- Some session;
            Hashtbl.add sessions_tbl key session;
            (session, false)
    with e ->
      Mutex.unlock mutex_tbl; raise e
  in
  (get_session, delete_session)

let do_session_data fn (sess : D.session) =
  let session = LinkedList.get sess in
  Mutex.lock session.mutex;
  try
    let r = fn session.data in
    Mutex.unlock session.mutex;
    r
  with e ->
    Mutex.unlock session.mutex;
    raise e

let set_session_data (sess : D.session) data cleanup =
  let session = LinkedList.get sess in
  Mutex.lock session.mutex;
  session.cleanup session.data;
  session.data <- data;
  session.cleanup <- cleanup;
  Mutex.unlock session.mutex

let set_session_cookie (sess : D.session) cname value =
  let session = LinkedList.get sess in
  Mutex.lock session.mutex;
  let cs = List.filter (fun (n,_) -> n <> cname) session.cookies in
  session.cookies <- (cname, value) :: cs;
  Mutex.unlock session.mutex

let mk_cookies (sess : D.session) c =
  let session = LinkedList.get sess in
  let max_age = Int64.of_float session.life_time in
  Mutex.lock session.mutex;
  let c = S.Cookies.create ~name:"SESSION_KEY" ~max_age
            ~same_site:`Strict session.key c in
  let c = S.Cookies.create ~name:"SESSION_ADDR" ~max_age
            ~same_site:`Strict session.addr c in
  let c = List.fold_left (fun c (name, value) ->
               S.Cookies.create ~name ~max_age
                 ~same_site:`Strict ~http_only:false value c) c session.cookies in
  Mutex.unlock session.mutex;
  c

let check
      ?(session_life_time=3600.0)
      ?(init=fun () -> NoData)
      ?(check=fun _ -> true)
      ?(error=(302,"index.html")) req =
  let cookies = S.Request.cookies req in
  let client = S.Request.client req in
  let key = Option.map Http_cookie.value
              (S.Request.get_cookie req "SESSION_KEY")
  in
  let (sess, old) = get_session ~session_life_time client key init in
  let session = LinkedList.get sess in
  try
    if not (check sess) then raise Exit;
    let cookies =
      if old then
        begin
          begin
            match (key, S.Request.get_cookie req "SESSION_ADDR") with
            | (Some key, Some addr) when
                   key = session.key &&
                     Http_cookie.value addr = session.addr -> ()
            | _ -> raise Exit
          end;
          let addr = addr_of_sock client.sock in
          if addr <> session.addr then raise Exit;
          cookies
        end
      else
        S.Cookies.delete_all cookies
    in
    let cookies = mk_cookies sess cookies in
    let gn = S.Response.update_headers
               (fun h -> S.Headers.set_cookies cookies h) in
    (req, gn)
  with Exit ->
    delete_session sess;
    let cookies = S.Cookies.delete_all cookies in
    let (code, redirect) = error in
    (req, (fun _ -> S.Response.update_headers
                      (fun h -> S.Headers.set_cookies cookies h)
                      (S.Response.set_header S.Headers.Location redirect
                         (S.Response.fail ~code "logout"))))
