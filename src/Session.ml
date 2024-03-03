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
  (*List.iter Async.close session.clients;*)
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

let get_session (type a) (req : a Request.t) =
  let client = Request.client req in
  match client.session with
  | Some l -> Some (LinkedList.get l)
  | None ->
     let key = Option.map Http_cookie.value
                 (Request.get_cookie req "SESSION_KEY")
     in
     match key with
     | None -> None
     | Some key ->
        Mutex.lock mutex_tbl;
        let r = Hashtbl.find_opt sessions_tbl key in
        Mutex.unlock mutex_tbl;
        Option.map LinkedList.get r

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
          Hashtbl.add sessions_tbl key session;
          Mutex.unlock mutex_tbl;
          (session_info, false)
  with e ->
    Mutex.unlock mutex_tbl; raise e

let get_session_data (sess : t) key =
  let l = Atomic.get sess.data in
  Util.search key l

let do_session_data : t -> (data -> 'a * data) -> 'a =
  fun (sess : t) fn ->
    Util.get_update_atomic sess.data fn

let set_session_data (sess : t) key x =
  do_session_data sess (fun l -> (), Util.add_replace key x l)

let remove_session_data sess key =
  do_session_data sess (fun l -> (), Util.remove key l)

let mk_cookies (session : t) filter c =
  let max_age = Int64.of_float session.life_time in
  let c = List.filter_map
            (fun c ->
              let name = Http_cookie.name c in
              if name = "SESSION_KEY" || name = "SESSION_ADDR" then
                None
              else match filter c with
                   | Some x -> Some x
                   | None -> Some (Http_cookie.expire c)) c
  in
  let c = Cookies.create ~name:"SESSION_KEY" ~max_age
            ~same_site:`Strict session.key c in
  let c = Cookies.create ~name:"SESSION_ADDR" ~max_age
            ~same_site:`Strict session.addr c in
  c

let start_check ?(session_life_time=3600.0)
      ?(check=fun (_:t) -> true)
      ?(filter=fun x -> Some x)
      ?(error=(bad_request, [])) req =
  let cookies = Request.cookies req in
  let client = Request.client req in
  let key = Option.map Http_cookie.value
              (Request.get_cookie req "SESSION_KEY")
  in
  let (session, old) = start_session ~session_life_time client key in
  try
    if not (check session) then raise Exit;
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
            | exception _ -> raise Exit
          end;
          let addr = Util.addr_of_sock client.sock in
          if addr <> session.addr then raise Exit;
          cookies
        end
      else
        begin
          Cookies.delete_all cookies
        end
    in
    (mk_cookies session filter cookies, session)
  with Exit ->
    delete_session session;
    let (code, headers) = error in
    let cookies = Cookies.delete_all cookies in
    Response.fail_raise ~headers ~cookies ~code "Delete session"

let delete_session
      ?(filter=fun x -> Some x)
      ?(error=(bad_request, [])) req =
  let check _ = false in
  ignore (start_check ~filter ~error ~check req);
  assert false

let filter
      ?(session_life_time=3600.0)
      ?(check=fun _ -> true)
      ?(filter=fun x -> Some x)
      ?(error=(bad_request, [])) req =
  let (cookies, _) = start_check ~session_life_time ~check ~filter
                       ~error req in
  let gn = Response.update_headers
             (fun h -> Headers.set_cookies cookies h) in
  (req, gn)
