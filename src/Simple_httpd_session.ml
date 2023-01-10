open Simple_httpd_domain

module S = Simple_httpd

let mk_cookies sess =
  let c = S.Cookies.empty in
  let c = S.Cookies.create ~name:"SESSION_KEY" sess.key c in
  let c = S.Cookies.create ~name:"SESSION_ADDR" sess.addr c in
  c

let addr_of_sock sock =
  match Unix.getsockname sock
  with ADDR_UNIX name -> "UNIX:" ^ name
     | ADDR_INET (addr, _) -> Unix.string_of_inet_addr addr

let new_session init client =
  let addr = addr_of_sock client.sock in
  let key = Digest.string (addr ^ string_of_int (Random.int 1_000_000_000)) in
  let sess = { addr; key; clients=[client]; mutex = Mutex.create ();
               data = init () }
  in
  (sess, mk_cookies sess)

exception BadSession

let check_session ?init fn req =
  let get k =
    match S.Request.get_cookie req k with
    | None   -> raise BadSession
    | Some c -> c
  in
  let client = S.Request.client req in
  match client.session with
  | None ->
     begin
       match init with
       | Some init ->
          let (sess, headers) = new_session init client in
          fn req sess headers
       | None -> NoData
     end
  | Some sess ->
    let key = Http_cookie.value (get "SESSION_KEY") in
    if key <> sess.key then raise BadSession;
    let addr =  Http_cookie.value (get "SESSION_ADDR") in
    if addr <> sess.addr then raise BadSession;
    let addr = addr_of_sock client.sock in
    if addr <> sess.addr then raise BadSession;
    fn req sess (mk_cookies sess)
