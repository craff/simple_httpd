open Simple_httpd_domain

module S = Simple_httpd_server

let addr_of_sock sock =
  match Unix.getsockname sock
  with ADDR_UNIX name -> "UNIX:" ^ name
     | ADDR_INET (addr, _) -> Unix.string_of_inet_addr addr

let get_session =
  let all_sessions = Hashtbl.create 1024 in
  let mutex = Mutex.create () in
  let get_session client key init =
    Mutex.lock mutex;
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
       let session = { addr; key; clients=[client]; mutex = Mutex.create ();
                       data } in
       client.session <- Some session;
       Hashtbl.add all_sessions key session;
       Mutex.unlock mutex;
       (session, false)
  in
  get_session

let mk_cookies sess =
  let c = S.Cookies.empty in
  let c = S.Cookies.create ~name:"SESSION_KEY" ~max_age:3600L (* FIXME *)
            ~same_site:`Strict sess.key c in
  let c = S.Cookies.create ~name:"SESSION_ADDR" ~max_age:3600L
            ~same_site:`Strict sess.addr c in
  c

let check ?init ?(remove=false) ?(error=(400, "bad session cookies")) req =
  try
    let get k =
      match S.Request.get_cookie req k with
      | None   -> raise Exit
      | Some c -> c
    in
    let client = S.Request.client req in
    let key = Option.map Http_cookie.value
                (S.Request.get_cookie req "SESSION_KEY")
    in
    let (session, old) = get_session client key init in
    if old then
      begin
        if key <> Some session.key then raise Exit;
        let addr =  Http_cookie.value (get "SESSION_ADDR") in
        if addr <> session.addr then raise Exit;
        let addr = addr_of_sock client.sock in
        if addr <> session.addr then raise Exit;
      end;
    let cookies = mk_cookies session in
    let cookies = if remove then S.Cookies.delete_all cookies else cookies in
    let gn = S.Response.update_headers
               (fun h -> S.Headers.set_cookies cookies h) in
    Ok gn
  with Exit -> Error error
