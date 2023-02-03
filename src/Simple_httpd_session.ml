open Simple_httpd_domain

module S = Simple_httpd_server

let addr_of_sock sock =
  match Unix.getsockname sock
  with ADDR_UNIX name -> "UNIX:" ^ name
     | ADDR_INET (addr, _) -> Unix.string_of_inet_addr addr

let get_session, delete_session =
  let all_sessions = Hashtbl.create 1024 in
  let mutex = Mutex.create () in
  let get_session client key init =
    Mutex.lock mutex;
    try
      match client.session with
      | Some s ->
         Mutex.unlock mutex;
         (s, true)
      | None ->
         let session = match key with
           | None -> None
           | Some key -> Hashtbl.find_opt all_sessions key
         in
         match session with
         | Some s ->
            client.session <- Some s;
            s.clients <- client :: s.clients;
            Mutex.unlock mutex;
            (s, true)
         | None ->
            let addr = addr_of_sock client.sock in
            let key = Digest.to_hex
                        (Digest.string (addr ^ string_of_int (Random.int 1_000_000_000))) in
            let data = match init with
              | None -> NoData
              | Some f -> f ()
            in
            let cleanup _ = () in
            let session = { addr; key; clients=[client]; mutex = Mutex.create ();
                            data; cleanup; cookies = [] } in
            client.session <- Some session;
            Hashtbl.add all_sessions key session;
            Mutex.unlock mutex;
            (session, false)
    with e -> Mutex.unlock mutex; raise e
  in
  let delete_session sess =
    Mutex.lock mutex;
    List.iter (fun cl -> cl.session <- None) sess.clients;
    Hashtbl.remove all_sessions sess.key;
    sess.cleanup sess.data;
    Mutex.unlock mutex
  in
  (get_session, delete_session)

let do_session_data fn sess =
  Mutex.lock sess.mutex;
  try
    let r = fn sess.data in
    Mutex.unlock sess.mutex;
    r
  with e ->
    Mutex.unlock sess.mutex;
    raise e

let set_session_data sess data cleanup =
  Mutex.lock sess.mutex;
  sess.cleanup sess.data;
  sess.data <- data;
  sess.cleanup <- cleanup;
  Mutex.unlock sess.mutex

let set_session_cookie sess cname value =
  Mutex.lock sess.mutex;
  let cs = List.filter (fun (n,_) -> n <> cname) sess.cookies in
  sess.cookies <- (cname, value) :: cs;
  Mutex.unlock sess.mutex

let mk_cookies sess c =
  let c = S.Cookies.create ~name:"SESSION_KEY" ~max_age:3600L (* FIXME *)
            ~same_site:`Strict sess.key c in
  let c = S.Cookies.create ~name:"SESSION_ADDR" ~max_age:3600L
            ~same_site:`Strict sess.addr c in
  let c = List.fold_left (fun c (name, value) ->
               S.Cookies.create ~name ~max_age:3600L
            ~same_site:`Strict ~http_only:false value c) c sess.cookies in
  c

let check ?init ?(remove=false) ?(error=(302,"index.html")) req =
  let cookies = S.Request.cookies req in
  let client = S.Request.client req in
  let key = Option.map Http_cookie.value
              (S.Request.get_cookie req "SESSION_KEY")
  in
  let (session, old) = get_session client key init in
  try
    if remove then
      begin
        delete_session session;
        let cookies = S.Cookies.delete_all cookies in
        let gn = S.Response.update_headers
                   (fun h -> S.Headers.set_cookies cookies h) in
        Ok gn
      end
    else
      begin
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
        let cookies = mk_cookies session cookies in
        let gn = S.Response.update_headers
                     (fun h -> S.Headers.set_cookies cookies h) in
        Ok gn
        end
  with Exit ->
    delete_session session;
    Error(error)
