open Response_code
module Mutex = Async.Mutex

type t = Async.session_info
type data = Key.data
type 'a key = 'a Key.key

let new_key
      ?(cleanup_delete=fun _ -> ())
      ?(cleanup_no_client=fun _ -> true)
      ?(save=output_value)
      ?(load=input_value)
      key_name =
  Key.new_key cleanup_no_client cleanup_delete save load key_name

module LinkedList = Util.LinkedList

(* table to search session by key *)
let sessions_tbl : (string, Async.session) Hashtbl.t = Hashtbl.create 1024
let mutex_tbl = Mutex.create ()
(* session orderer by expiration data *)
let sessions_list = LinkedList.create ()
let mutex_list = Mutex.create ()

let save_session ch session =
  let open Async in
  output_value ch true;
  output_value ch session.key;
  output_value ch session.life_time;
  output_value ch session.last_refresh;
  Key.save ch (Atomic.get session.data)

let load_session ch =
  let key  = input_value ch in
  let life_time = input_value ch in
  let last_refresh = input_value ch in
  let data = Atomic.make (Key.load ch) in
  let session_info =
    Async.({ key; clients= Atomic.make []
             ; data; cell = LinkedList.fake_cell
             ; life_time; last_refresh})
  in
  let session = LinkedList.add_first session_info sessions_list in
  session_info.cell <- session;
  Hashtbl.replace sessions_tbl key session

let save_name = "SESSION"

let save_sessions ch =
  Mutex.lock mutex_tbl;
  Mutex.lock mutex_list;
  output_value ch (save_name, 1);
  Util.LinkedList.iter (save_session ch) sessions_list;
  output_value ch false;
  Mutex.unlock mutex_tbl;
  Mutex.unlock mutex_list


let load_sessions (name, version) ch =
  Mutex.lock mutex_tbl;
  Mutex.lock mutex_list;
  assert (name = save_name);
  let load =
    match version with
    | 1 -> load_session
    | _ -> assert false
  in
  while input_value ch do
    load ch
  done;
  Mutex.unlock mutex_tbl;
  Mutex.unlock mutex_list

let delete_session session =
  Async.Mutex.use mutex_tbl (fun () ->
      Hashtbl.remove sessions_tbl session.Async.key);
  let fn clients =
    List.iter (fun cl -> Async.set_session cl) clients;
    []
  in
  let _ = Util.update_atomic session.clients fn in
  Mutex.use mutex_list (fun () ->
      LinkedList.remove_cell session.cell sessions_list);
  List.iter (fun cl -> Async.Client.close cl)
    (Atomic.get session.clients);
  Key.cleanup_delete (Atomic.get session.data)

let refresh session =
  let now = Unix.gettimeofday () in
  Mutex.use mutex_list (fun () ->
      LinkedList.move_first session sessions_list;
      (LinkedList.get session).last_refresh <- now;
      while  let l = LinkedList.tail sessions_list in
             let sess = LinkedList.get l in
             LinkedList.is_cell l && now -. sess.last_refresh > sess.life_time do
        let l = LinkedList.tail sessions_list in
        LinkedList.remove_cell l sessions_list;
      done)

type cookie_policy =
  { path : string
  ; base : string
  ; life : float
  ; filter : Cookies.cookie -> Cookies.cookie option }

let default_cookie_policy =
  { path = "/"
  ; base = "Session"
  ; life = 3600.0
  ; filter = fun _ -> None }

let prefix_sec info =
  if info.path = "/" then "Host" else "Secure"
let session_key info =
  Printf.sprintf "__%s-%s-Key" (prefix_sec info) info.base

let get_client_session client key =
  match Async.(client.session) with
  | Some l -> Some l
  | None ->
     match key with
     | None -> None
     | Some key ->
        Mutex.use mutex_tbl (fun () -> Hashtbl.find_opt sessions_tbl key)

let get_session (type a) ?(cookie_policy=default_cookie_policy) (req : a Request.t) =
  let key = Option.map Cookies.value
              (Request.get_cookie req (session_key cookie_policy))
  in
  let client = Request.client req in
  match client.session with
  | Some session ->
     Some (LinkedList.get session)
  | None ->
     let session = get_client_session client key in
     match session with
     | Some session ->
        let session_info = LinkedList.get session in
        let fn clients = client :: clients in
        let _ = Util.update_atomic session_info.clients fn in
        Some session_info
     | _ ->
        None

let start_session ?(session_life_time=3600.0) client key =
  match client.Async.session with
  | Some s ->
     refresh s;
     (LinkedList.get s, true)
  | None ->
     let session =
       Mutex.use mutex_tbl (fun () ->
           match key with
           | None -> None
           | Some key -> Hashtbl.find_opt sessions_tbl key)
     in
     match session with
       Some session ->
        Async.set_session ~session client;
        let session_info = LinkedList.get session in
        let fn clients = client :: clients in
        let _ = Util.update_atomic session_info.clients fn in
        refresh session;
        (session_info, true)
     | None ->
        let key = Digest.to_hex
                      (Digest.string
                         ((string_of_int (Random.full_int 1_000_000_000)) ^
                            (string_of_int (Random.full_int 1_000_000_000)) ^
                              (string_of_int (Random.full_int 1_000_000_000)))
                      ) in
        let data = Atomic.make Key.empty in
        let now = Unix.gettimeofday () in
        let session_info =
          Async.({ key; clients= Atomic.make [client]
                   ; data; cell = LinkedList.fake_cell
                   ; life_time = session_life_time
                   ; last_refresh = now })
        in
        let session =
          Mutex.use mutex_list (fun () ->
            LinkedList.add_first session_info sessions_list)
        in
        session_info.cell <- session;
        Async.set_session ~session client;
        Mutex.use mutex_tbl (fun () ->
            Hashtbl.replace sessions_tbl key session);
        (session_info, false)

let get_session_data (sess : t) key =
  let l = Atomic.get sess.data in
  try Some (Key.search key l) with Not_found -> None

let get_session_key sess = sess.Async.key

let do_session_data : t -> (data -> 'a * data) -> 'a =
  fun (sess : t) fn ->
    Util.get_update_atomic sess.data fn

let set_session_data (sess : t) key x =
  do_session_data sess (fun l -> (), Key.add_replace key x l)

let remove_session_data sess key =
  do_session_data sess (fun l -> (), Key.remove key l)


let mk_cookies (session : t)  cookie_policy cs =
  let session_key = session_key cookie_policy in
  let path = cookie_policy.path in
  let max_age = Int64.of_float cookie_policy.life in
  let cs = Cookies.(create ~name:session_key ~max_age ~path ~secure:true
            ~same_site:Strict session.key cs) in
  cs

let select_cookies ?(delete=false) ?create cookie_policy cookies =
  let session_key = session_key cookie_policy in
  let cookies =
    let open Cookies in
    List.filter_map
    (fun c ->
      let name = c.name in
      if name = session_key then
        begin
          let max_age =
            if delete then Some 0L else
              let max_age = Int64.of_float cookie_policy.life in
              Some max_age
          in
          let value = if delete then "" else c.value in
          let c = { c with
                    path = Some cookie_policy.path;
                    secure = true ;
                    same_site = Strict;
                    max_age;
                    value ;
                  }
          in
          Some c
        end
      else
        match cookie_policy.filter c with
        | Some c when delete -> Some (expire c)
        | opt -> opt) cookies
  in
  match create with
  | Some session -> mk_cookies session cookie_policy cookies
  | None         -> cookies

exception Bad_session_cookie

let check_session_cookie ?(cookie_policy=default_cookie_policy) ?(create=false) req =
  let session =
    match get_session ~cookie_policy req with
    | None -> raise Bad_session_cookie
    | Some session -> session
  in
  let cookies = Request.cookies req in
  let session_key = session_key cookie_policy in
  let key = Option.map Cookies.value
              (Request.get_cookie req session_key)
  in

  match key with
  | Some key when key = session.key ->
     Some session
  | None when cookies = [] && create ->
     None
  | _ -> raise Bad_session_cookie
  | exception _ -> raise Bad_session_cookie

let start_check
      ?(create=false)
      ?(check=fun (_:t) -> true)
      ?(cookie_policy=default_cookie_policy)
      ?(nosession=Exit)
      ?(error=(bad_request, [])) req =
  let session_life_time = cookie_policy.life in
  let cookies = Request.cookies req in
  let session_key = session_key cookie_policy in
  let key = Option.map Cookies.value
              (Request.get_cookie req session_key)
  in
  let client = Request.client req in

  let exception Bad of t in
  try
    let session, delete =
      match check_session_cookie ~cookie_policy ~create req with
      | Some session -> (session, false)
      | None when create ->
         start_session ~session_life_time client key
      | _ -> raise nosession
      | exception Bad_session_cookie when create ->
         start_session ~session_life_time client key
      | exception e ->
         Log.f (Exc 0)
           (fun k -> k "Bad session cookie and create session not allowed: %s"
                       (Printexc.to_string e));
         raise nosession
    in
    let bad () = raise (Bad session) in
    if not (check session) then bad ();
    (select_cookies ~delete ~create:session cookie_policy cookies, session)
  with Bad session ->
        delete_session session;
        let (code, headers) = error in
        let cookies = select_cookies ~delete:true cookie_policy (Request.cookies req) in
        Response.fail_raise ~headers ~cookies ~code "Delete session"
     | Exit ->
        let (code, headers) = error in
        Response.fail_raise ~headers ~code "No session"

let delete_session ?(cookie_policy=default_cookie_policy) req =
  let session = get_session req in
  begin
    match session with
    | None -> ()
    | Some session -> delete_session session
  end;
  select_cookies ~delete:true cookie_policy (Request.cookies req)

let filter
      ?(check=fun _ -> true)
      ?(cookie_policy=default_cookie_policy)
      ?(error=(bad_request, [])) req =
  let (cookies, _) = start_check ~check ~cookie_policy ~error req in
  let gn = Response.update_headers
             (fun h -> Headers.set_cookies cookies h) in
  (req, gn)
