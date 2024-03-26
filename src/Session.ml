open Response_code

type t = Async.session_info
type data = Util.data
type 'a key = 'a Util.key

let new_key () = Util.new_key (fun _ -> ())
let new_key_with_cleanup = Util.new_key

module LinkedList = Util.LinkedList

(* table to search session by key *)
let sessions_tbl : (string, Async.session) Hashtbl.t = Hashtbl.create 1024
let mutex_tbl = Mutex.create ()
(* session orderer by expiration data *)
let sessions_list = LinkedList.create ()
let mutex_list = Mutex.create ()

let delete_session session =
  Mutex.lock mutex_tbl;
  Hashtbl.remove sessions_tbl session.Async.key;
  Mutex.unlock mutex_tbl;
  let fn clients =
    List.iter (fun cl -> Async.set_session cl) clients;
    []
  in
  Util.update_atomic session.clients fn;
  Mutex.lock mutex_list;
  (try LinkedList.remove_cell session.cell sessions_list;
       Mutex.unlock mutex_list
   with e ->
     Mutex.unlock mutex_list; raise e);
  List.iter Async.close (Atomic.get session.clients);
  Util.cleanup (Atomic.get session.data)

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

type cookie_policy =
  { path : string
  ; base : string
  ; life : float
  ; filter : Http_cookie.t -> Http_cookie.t option }

let default_cookie_policy =
  { path = "/"
  ; base = "Session"
  ; life = 3600.0
  ; filter = fun c ->
             let name = Http_cookie.name c in
             if name.[0] = '_' then None else Some c }


let prefix_sec info =
  if info.path = "/" then "Host" else "Secure"
let session_key info =
  Printf.sprintf "__%s-%s-Key" (prefix_sec info) info.base
let session_adr info =
  Printf.sprintf "__%s-%s-Adr" (prefix_sec info) info.base

let get_client_session client key =
  match Async.(client.session) with
  | Some l -> Some l
  | None ->
     match key with
     | None -> None
     | Some key ->
        Mutex.lock mutex_tbl;
        let r = Hashtbl.find_opt sessions_tbl key in
        Mutex.unlock mutex_tbl;
        Option.map LinkedList.get r

let get_session (type a) ?(cookie_policy=default_cookie_policy) (req : a Request.t) =
  let key = Option.map Http_cookie.value
              (Request.get_cookie req (session_key cookie_policy))
  in
  get_client_session (Request.client req) key

let start_session ?(session_life_time=3600.0) client key =
  try
    match client.Async.session with
    | Some s ->
       refresh s;
       (LinkedList.get s, true)
    | None ->
       Mutex.lock mutex_tbl;
       let session = match key with
         | None -> None
         | Some key -> Hashtbl.find_opt sessions_tbl key
       in
       match session with
       | Some session ->
          Async.set_session ~session client;
          Mutex.unlock mutex_tbl;
          let session_info = LinkedList.get session in
          let fn clients = client :: clients in
          Util.update_atomic session_info.clients fn;
          refresh session;
          (session_info, true)
       | None ->
          Mutex.unlock mutex_tbl;
          let addr = Util.addr_of_sock client.sock in
          let key = Digest.to_hex
                      (Digest.string (addr ^ string_of_int (Random.int 1_000_000_000))) in
          let data = Atomic.make Util.empty in
          let now = Unix.gettimeofday () in
          let session_info =
            Async.({ addr; key; clients= Atomic.make [client]
                     ; data; cell = LinkedList.fake_cell
                     ; life_time = session_life_time
                     ; last_refresh = now })
          in
          Mutex.lock mutex_list;
          let session = LinkedList.add_first session_info sessions_list in
          Mutex.unlock mutex_list;
          session_info.cell <- session;
          Async.set_session ~session client;
          Mutex.lock mutex_tbl;
          Hashtbl.replace sessions_tbl key session;
          Mutex.unlock mutex_tbl;
          (session_info, false)
  with e ->
    Mutex.unlock mutex_tbl; raise e

let get_session_data (sess : t) key =
  let l = Atomic.get sess.data in
  try Some (Util.search key l) with Not_found -> None

let do_session_data : t -> (data -> 'a * data) -> 'a =
  fun (sess : t) fn ->
    Util.get_update_atomic sess.data fn

let set_session_data (sess : t) key x =
  do_session_data sess (fun l -> (), Util.add_replace key x l)

let remove_session_data sess key =
  do_session_data sess (fun l -> (), Util.remove key l)


let mk_cookies (session : t)  cookie_policy cs =
  let session_key = session_key cookie_policy in
  let session_adr = session_adr cookie_policy in
  let path = cookie_policy.path in
  let max_age = Int64.of_float session.life_time in
  let cs = List.filter_map
            (fun c ->
              let name = Http_cookie.name c in
              if name = session_key || name = session_adr then
                None
              else
                Option.map
                  (fun c -> match Http_cookie.update_max_age (Some max_age) c
                            with Ok c -> c
                               | Error _ -> assert false)
                  (cookie_policy.filter c))
            cs
  in
  let cs = Cookies.create ~name:session_key ~max_age ~path ~secure:true
            ~same_site:`Strict session.key cs in
  let cs = Cookies.create ~name:session_adr ~max_age ~path ~secure:true
            ~same_site:`Strict session.addr cs in
  cs

let select_cookies cookie_policy req =
  let session_key = session_key cookie_policy in
  let session_adr = session_adr cookie_policy in
  List.filter_map
    (fun c ->
      let name = Http_cookie.name c in
      if name = session_key || name = session_adr then
        Some c
      else cookie_policy.filter c) (Request.cookies req)

let start_check
      ?(create=true)
      ?(check=fun (_:t) -> true)
      ?(cookie_policy=default_cookie_policy)
      ?(error=(bad_request, [])) req =
  let session_life_time = cookie_policy.life in
  let cookies = Request.cookies req in
  let client = Request.client req in
  let session_key = session_key cookie_policy in
  let session_adr = session_adr cookie_policy in
  let key = Option.map Http_cookie.value
              (Request.get_cookie req session_key)
  in
  let exception Bad of t in
  try
    let (session, old) =
      if create then start_session ~session_life_time client key
      else match get_client_session client key with
           | None -> raise Exit
           | Some session ->
              let session = LinkedList.get session in
              (session, true)
    in
    let bad () = raise (Bad session) in
    if not (check session) then bad ();
    let cookies =
      if old then
        begin
          let session, cookies =
            match (key, Request.get_cookie req session_adr) with
            | (Some key, Some addr) when
                   key = session.key &&
                     Http_cookie.value addr = session.addr -> (session, cookies)
            | (None, None) when cookies = [] -> (session, cookies)
            | _ ->
               delete_session session;
               if create then (fst (start_session ~session_life_time client key), [])
               else raise Exit
            | exception _ -> bad ()
          in
          let addr = Util.addr_of_sock client.sock in
          if addr <> session.addr then bad ();
          cookies
        end
      else
        begin
          Cookies.delete_all cookies
        end
    in
    (mk_cookies session cookie_policy cookies, session)
  with Bad session ->
        delete_session session;
        let (code, headers) = error in
        let cookies = Cookies.delete_all (select_cookies cookie_policy req) in
        Response.fail_raise ~headers ~cookies ~code "Delete session"
     | Exit ->
        let (code, headers) = error in
        let cookies = Cookies.delete_all (select_cookies cookie_policy req) in
        Response.fail_raise ~headers ~cookies ~code "Delete session"

let delete_session ?(cookie_policy=default_cookie_policy) req =
  let session = get_session req in
  begin
    match session with
    | None -> ()
    | Some session -> delete_session session
  end;
  Cookies.delete_all (select_cookies cookie_policy req)

let filter
      ?(check=fun _ -> true)
      ?(cookie_policy=default_cookie_policy)
      ?(error=(bad_request, [])) req =
  let (cookies, _) = start_check ~check ~cookie_policy ~error req in
  let gn = Response.update_headers
             (fun h -> Headers.set_cookies cookies h) in
  (req, gn)
