type same_site = None | Lax | Strict
type cookie = {
    name : string;
    value : string;
    path : string option;
    domain : string option;
    expires : Unix.tm option;
    max_age : int64 option;
    secure : bool;
    http_only : bool;
    same_site : same_site;
    extension : string option;
  }

type t = cookie list

let empty = []

exception Invalid_cookie of string

let add c cookies =
  let name = c.name in
  let cookies = List.filter (fun c -> c.name  <> name) cookies in
  c :: cookies

let create : ?path:string ->
             ?domain:string ->
             ?expires:Unix.tm ->
             ?max_age:int64 ->
             ?secure:bool ->
             ?http_only:bool ->
             ?same_site:same_site ->
             ?extension:string ->
             name:string ->
             string -> t -> t =
  fun ?path ?domain ?expires ?max_age ?(secure=false) ?(http_only=true)
      ?(same_site=Strict) ?extension ~name value cookies ->
  let c =
    { path ;
      domain;
      expires;
      max_age;
      secure;
      http_only;
      same_site;
      extension;
      name;
      value }
  in add c cookies

let get name cookies =
  List.find (fun c -> c.name = name) cookies

let char_name = function
    | '\x00' .. '\x1F' | '\x7F' -> false (* CONTROL chars *)
    | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '['
    | ']' | '?' | '=' | '{' | '}' | ' ' ->
        false (* SEPARATOR chars *)
    | _ -> true

let char_value = function
    | '\x21'
    | '\x23' .. '\x2B'
    | '\x2D' .. '\x3A'
    | '\x3C' .. '\x5B'
    | '\x5D' .. '\x7E' ->
        true
    | _ -> false


let parse str =
  let res = ref [] in
  let len = String.length str in
  let i = ref 0 in
  while !i < len do
    let skip_space () =
      while !i <= len && str.[!i] = ' ' do
        incr i
      done
    in
    skip_space ();
    let bgn = !i in
    while !i <= len && char_name str.[!i] do
      incr i
    done;
    let name = String.sub str bgn (!i - bgn) in
    skip_space ();
    if str.[!i] = '=' then
      begin
        incr i;
        skip_space ();
      end
    else
      raise (Invalid_cookie str);
    let quote = if str.[!i] = '"' then (incr i; true) else false in
    let bgn = !i in
    while !i <= len && char_value str.[!i] do
      incr i
    done;
    let value = String.sub str bgn (!i - bgn) in
    if quote then
      if str.[!i] = '"' then incr i
      else raise (Invalid_cookie str);
    res := create ~name value !res;
    skip_space ();
    if str.[!i] = ';' then (incr i; skip_space ())
  done;
  !res

let same_site_to_string = function
  | None -> "None"
  | Lax -> "Lax"
  | Strict -> "Strict"

let date_to_string tm =
  let open Unix in
  let weekday =
    match tm.tm_wday with
    | 0 -> "Sun"
    | 1 -> "Mon"
    | 2 -> "Tue"
    | 3 -> "Wed"
    | 4 -> "Thu"
    | 5 -> "Fri"
    | 6 -> "Sat"
    | _ -> assert false
  in
  let month =
    match tm.tm_mon with
    | 0 -> "Jan"
    | 1 -> "Feb"
    | 2 -> "Mar"
    | 3 -> "Apr"
    | 4 -> "May"
    | 5 -> "Jun"
    | 6 -> "Jul"
    | 7 -> "Aug"
    | 8 -> "Sep"
    | 9 -> "Oct"
    | 10 -> "Nov"
    | 11 -> "Dec"
    | _ -> assert false
  in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT" weekday tm.tm_mday
    month tm.tm_year tm.tm_hour tm.tm_min tm.tm_sec

let cookie_to_string c =
  let module O = Option in
  let buf = Buffer.create 50 in
  let add_str fmt = Format.ksprintf (Buffer.add_string buf) fmt in
  add_str "%s=%s" c.name c.value;
  O.iter (fun path -> add_str "; Path=%s" path) c.path;
  O.iter (fun d -> add_str "; Domain=%s" d) c.domain;
  O.iter
    (fun expires -> add_str "; Expires=%s" @@ date_to_string expires)
    c.expires;
  O.iter (fun max_age -> add_str "; Max-Age=%Ld" max_age) c.max_age;
  if c.secure then add_str "; Secure";
  if c.http_only then add_str "; HttpOnly";
  add_str "; SameSite=%s" (same_site_to_string c.same_site);
  O.iter (fun extension -> add_str "; %s" extension) c.extension;
  Buffer.contents buf

let expire cookie =
  {
    cookie with
    value = "";
    expires = None;
    max_age = Some (-1L);
    extension = None;
  }

let value c = c.value
let name c = c.name
