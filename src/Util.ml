
let _ =
  if Sys.(os_type <> "Unix") then failwith "unsupported OS"

let file_descr_to_int : Unix.file_descr -> int = Obj.magic

let file_descr_of_int : int -> Unix.file_descr = Obj.magic

let remove_first cond l =
  let[@tail_mod_cons] rec fn = function
    | x::l -> if cond x then l else x :: fn l
    | [] -> raise Not_found
  in
  fn l

external rlimit_cur : unit -> int = "caml_rlimit_cur"

let maxfd : int = rlimit_cur ()

external raw_single_write : (int [@untagged]) -> Bytes.t ->
                            (int [@untagged]) -> (int [@untagged]) -> (int [@untagged])
  = "caml_byte_fast_single_write" "caml_fast_single_write" [@@noalloc]

external write_error : unit -> 'a = "caml_write_error"

let single_write fd buf ofs len =
  if ofs < 0 || len < 0 || ofs+len > Bytes.length buf then
    invalid_arg "single_write";
  let ret = raw_single_write (file_descr_to_int fd) buf ofs len in
  if ret == -1 then write_error();
  ret

external raw_read : (int [@untagged]) -> Bytes.t ->
                    (int [@untagged]) -> (int [@untagged]) -> (int [@untagged])
  = "caml_byte_fast_read" "caml_fast_read" [@@noalloc]

external read_error : unit -> 'a = "caml_read_error"

let read fd buf ofs len =
  if ofs < 0 || len < 0 || ofs+len > Bytes.length buf then
    invalid_arg "read";
  let ret = raw_read (file_descr_to_int fd) buf ofs len in
  if ret == -1 then read_error();
  ret

(* test utils *)
(*$inject
  let pp_res f = function Ok x -> f x | Error e -> e
  let pp_res_query = (Q.Print.(pp_res (list (pair string string))))
  let err_map f = function Ok x-> Ok (f x) | Error e -> Error e
  let sort_l l = List.sort compare l
  let eq_sorted a b = (=) (err_map sort_l a)(err_map sort_l b)
  let is_ascii_char c = Char.code c < 128
*)

external raw_setsockopt_cork : (int [@untagged]) -> (bool [@untagged]) -> (int [@untagged])
  = "caml_byte_setsockopt_cork" "caml_setsockopt_cork" [@@noalloc]

let setsockopt_cork fd v =
  if raw_setsockopt_cork (file_descr_to_int fd) v < 0 then
    raise Unix.(Unix_error(ENOTSOCK,"setsockopt_cork",""))

external raw_sendfile : (int [@untagged]) -> (int [@untagged]) ->
                        (int [@untagged]) -> (int [@untagged]) -> (int [@untagged])
  = "caml_byte_sendfile" "caml_sendfile" [@@noalloc]

external sendfile_error : unit -> 'a = "caml_sendfile_error"

external raw_ssl_sendfile : Ssl.socket -> (int [@untagged])
                            -> (int [@untagged]) -> (int [@untagged]) -> (int [@untagged])
  = "caml_byte_ssl_sendfile" "caml_ssl_sendfile" [@@noalloc]

external ssl_nonblock : Ssl.context -> unit
  = "caml_ssl_nonblock" [@@noalloc]

external get_error : Ssl.socket -> int -> Ssl.ssl_error = "ocaml_ssl_get_error_code"
    [@@noalloc]

let get_socket_error : ?default:Unix.error -> Unix.file_descr -> Unix.error option =
  fun ?default sock ->
  match Unix.(getsockopt_error sock) with
  | None -> default
  | e -> e
  | exception Unix.Unix_error(e,_,_) -> Some e

let sendfile out in_ offset count =
  let ret = raw_sendfile (file_descr_to_int out)
              (file_descr_to_int in_) offset count in
  if ret == -1 then sendfile_error ();
  ret

let ssl_sendfile out in_ offset count =
  let ret = raw_ssl_sendfile out (file_descr_to_int in_) offset count in
  if ret <= 0 then raise Ssl.(Write_error (get_error out ret));
  ret

let percent_encode ?(skip=fun _->false) s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | c when skip c -> Buffer.add_char buf c
      | ( '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+'
        | ',' | '/' | ':' | ';' | '=' | '?' | '@' | '[' | ']' | '~')
        as c ->
        Printf.bprintf buf "%%%X" (Char.code c)
      | c when let code = Char.code c in code > 127 || code <= 32 ->
        Printf.bprintf buf "%%%02X" (Char.code c)
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

(*$= & ~printer:(fun s->s)
  "hello%20world" (percent_encode "hello world")
  "%23%25^%24%40^%40" (percent_encode "#%^$@^@")
  "a%20ohm%2B5235%25%26%40%23%20---%20_" (percent_encode "a ohm+5235%&@# --- _")
  "%0A" (percent_encode "\n")
*)

let hex_int (s:string) : int = Scanf.sscanf s "%x" (fun x->x)

let percent_decode (s:string) =
  let buf = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i < String.length s do
    match String.get s !i with
    | '%' ->
       if !i+2 < String.length s then (
         begin match hex_int @@ String.sub s (!i+1) 2 with
         | n -> Buffer.add_char buf (Char.chr n)
         | exception _ -> invalid_arg "percent_decode"
         end;
         i := !i + 3;
       ) else (
         raise Exit (* truncated *)
       )
    | '+' -> Buffer.add_char buf ' '; incr i (* for query strings *)
    | c -> Buffer.add_char buf c; incr i
  done;
  Buffer.contents buf

(*$= & ~printer:(fun s ->s)
  "?" (percent_decode @@ percent_encode "?")
  "\n" (percent_decode @@ percent_encode "\n")
  "\r" (percent_decode @@ percent_encode "\r")
  "\t" (percent_decode @@ percent_encode "\t")
  "abs def&;\n cou=cou[](){}" (percent_decode @@ percent_encode "abs def&;\n cou=cou[](){}")
*)


(*$QR & ~count:1_000 ~long_factor:20
    Q.string (fun s ->
        String.iter (fun c -> Q.assume @@ is_ascii_char c) s;
        percent_decode (percent_encode s) = s ||
          Q.Test.fail_report "invalid percent encoding")
*)

exception Invalid_query of string

let find_q_index_ s = String.index s '?'

let get_non_query_path s =
  match find_q_index_ s with
  | i -> String.sub s 0 i
  | exception Not_found -> s

let get_query s : string =
  match find_q_index_ s with
  | i -> String.sub s (i+1) (String.length s-i-1)
  | exception Not_found -> ""

let split_query s = get_non_query_path s, get_query s

let split_on_slash s : _ list =
  let l = ref [] in
  let n = String.length s in
  let j = ref n in
  for i = n - 1 downto 0 do
    if String.unsafe_get s i = '/' then
      begin
        let len = !j - i - 1 in
        if len > 0 then
          l := String.sub s (i+1) len :: !l;
        j := i
      end
  done;
  if !j = n then [s] else
    if !j > 0 then String.sub s 0 !j :: !l
    else !l

(*$= & ~printer:Q.Print.(list string)
  ["a"; "b"] (split_on_slash "/a/b")
  ["a"; "b"] (split_on_slash "/a/b/")
  ["a"; "b"] (split_on_slash "/a/b//")
  ["coucou"; "lol"] (split_on_slash "/coucou/lol")
  ["a"; "b"; "c"] (split_on_slash "/a/b//c/")
  ["a"; "b"] (split_on_slash "//a/b/")
  ["a"] (split_on_slash "/a//")
  [] (split_on_slash "/")
  [] (split_on_slash "//")
*)

let parse_query s : _ list =
  let pairs = ref [] in
  let is_sep_ = function '&' | ';' -> true | _ -> false in
  let i = ref 0 in
  let j = ref 0 in
  try
    let percent_decode s =
      try percent_decode s with _ -> raise (Invalid_query "bad percent encoding")
    in
    let parse_pair () =
      let eq = String.index_from s !i '=' in
      let k = percent_decode @@ String.sub s !i (eq- !i) in
      let v = percent_decode @@ String.sub s (eq+1) (!j-eq-1) in
      pairs := (k,v) :: !pairs;
    in
    while !i < String.length s do
      while !j < String.length s && not (is_sep_ (String.get s !j)) do incr j done;
      if !j < String.length s then (
        assert (is_sep_ (String.get s !j));
        parse_pair();
        i := !j+1;
        j := !i;
      ) else (
        parse_pair();
        i := String.length s; (* done *)
      )
    done;
    !pairs
  with
  | Invalid_argument _ | Not_found | Failure _ as e ->
    raise (Invalid_query (Printexc.to_string e))

(*$= &
  ["c", "d"; "a", "b"] (parse_query "a=b&c=d")
*)

(*$QR & ~long_factor:20 ~count:1_000
    Q.(small_list (pair string string))
      (fun l ->
        List.iter (fun (a,b) ->
            Q.assume (a<>"" && b<>"" );
            String.iter (fun c -> Q.assume @@ is_ascii_char c) a;
            String.iter (fun c -> Q.assume @@ is_ascii_char c) b;
          ) l;
        let s = String.concat "&"
            (List.map (fun (x,y) -> percent_encode x ^"="^percent_encode y) l) in
        l = parse_query s)
*)

let pp_date date =
  let open Unix in
  let day = match date.tm_wday with
    | 0 -> "Sun"
    | 1 -> "Mon"
    | 2 -> "Tue"
    | 3 -> "Wed"
    | 4 -> "Thu"
    | 5 -> "Fri"
    | 6 -> "Sat"
    | _ -> invalid_arg "print_date"
  in
  let month = match date.tm_mon with
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
    |10 -> "Nov"
    |11 -> "Dec"
    | _ -> invalid_arg "print_date"
  in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT"
    day date.tm_mday month (date.tm_year+1900) date.tm_hour date.tm_min date.tm_sec

let date_of_epoch time =
  let tm = (Unix.gmtime time) in
  pp_date tm

let _ = Unix.putenv "TZ" "UTC" (* we do not use localtime, only gmtime *)

let date_to_epoch str =
  Scanf.sscanf str "%s@, %d %s %d %d:%d:%d GMT"
    (fun _ day month year h m s ->
      let month = match month with
        | "Jan" -> 0
        | "Feb" -> 1
        | "Mar" -> 2
        | "Apr" -> 3
        | "May" -> 4
        | "Jun" -> 5
        | "Jul" -> 6
        | "Aug" -> 7
        | "Sep" -> 8
        | "Oct" -> 9
        | "Nov" -> 10
        | "Dec" -> 11
        | _     -> invalid_arg "date_of_string: bad month"
      in
      let tm =
        Unix.{ tm_sec = s; tm_min = m; tm_hour = h; tm_mday = day;
               tm_mon = month; tm_year = year - 1900;
               tm_wday = 0; tm_yday = 0; tm_isdst = false }
      in
      fst (Unix.mktime tm))

module LinkedList = struct

  type 'a cell =
    | Nil
    | Cons of { v : 'a
              ; mutable next : 'a cell
              ; mutable prev : 'a cell }

  type 'a t = { mutable head : 'a cell
              ; mutable tail : 'a cell }

  let create () = { head = Nil; tail = Nil }

  let new_cell v = Cons { v; next = Nil; prev = Nil }
  let fake_cell = Nil

  let is_cell = function
    | Nil -> false
    | Cons _ -> true

  let is_empty l = not (is_cell l.head)

  let get = function
    | Nil -> raise Not_found
    | Cons { v; _ } -> v

  let head l = l.head
  let tail l = l.tail
  let next l = match l with
    | Nil -> raise Not_found
    | Cons r -> r.next
  let prev l = match l with
    | Nil -> raise Not_found
    | Cons r -> r.prev

  let size l =
    let rec fn acc = function
      | Nil -> acc
      | Cons { next; _ } -> fn (acc+1) next
    in
    fn 0 l.head

  let add_first v l =
    let cell = Cons { v; next = l.head; prev = Nil } in
    (match l.head with
    | Nil    -> assert (l.tail = Nil);
                l.head <- cell; l.tail <- cell
    | Cons r -> r.prev <- cell; l.head <- cell);
    cell

  let add_last v l =
    let cell = Cons { v; next = Nil; prev = l.tail } in
    (match l.tail with
    | Nil    -> assert (l.head = Nil);
                l.head <- cell; l.tail <- cell
    | Cons r -> r.next <- cell; l.tail <- cell);
    cell

  let remove_cell : 'a cell -> 'a t -> unit =
    fun c l ->
      match c with
      | Nil -> ()
      | Cons r ->
         (match r.prev with
          | Nil -> assert (l.head == c); l.head <- r.next
          | Cons p -> p.next <- r.next);
         (match r.next with
          | Nil -> assert (l.tail == c); l.tail <- r.prev
          | Cons n -> n.prev <- r.prev);
         r.next <- Nil; r.prev <- Nil

  let insert_first : 'a cell -> 'a t -> unit =
    fun c l ->
          match c with
      | Nil -> ()
      | Cons r ->
         assert (r.next = Nil);
         assert (r.prev = Nil);
         r.next <- l.head;
         (match l.head with
          | Nil -> l.tail <- c
          | Cons r' -> r'.prev <- c);
         l.head <- c

  let move_first : 'a cell -> 'a t -> unit =
    fun c l -> remove_cell c l; insert_first c l

  let insert_last : 'a cell -> 'a t -> unit =
    fun c l ->
          match c with
      | Nil -> ()
      | Cons r ->
         assert (r.next = Nil);
         assert (r.prev = Nil);
         r.prev <- l.tail;
         (match l.tail with
          | Nil -> l.head <- c
          | Cons r' -> r'.next <- c);
         l.tail <- c

  let move_last : 'a cell -> 'a t -> unit =
    fun c l -> remove_cell c l; insert_last c l

  type 'a prev = Cell of 'a cell | Root of 'a t

  let search_and_remove fn fn' l =
    let rec gn prev = function
      | Nil -> ()
      | Cons{ v; next; _ } as cell ->
         if fn v then
           begin
             let cell = match prev with
              | Root r -> r.head <- next; Nil
              | Cell (Cons c as cell) -> c.next <- next; cell
              | Cell Nil -> assert false
             in
             begin
               match next with
               | Nil -> l.tail <- cell
               | Cons c -> c.prev <- cell
             end;
             fn' v;
           end
         else
           gn (Cell cell) next
    in
    gn (Root l) l.head

  let iter fn { head; _ } =
    let rec loop cell =
      match cell with
      | Nil -> ()
      | Cons{v; next; _} -> fn v; loop next
    in
    loop head

end

let update_atomic a fn =
  let rec gn () =
    let old = Atomic.get a in
    let b = fn old in
    if Atomic.compare_and_set a old b then b
    else gn ()
  in
  gn ()

let get_update_atomic a fn =
  let rec gn () =
    let old = Atomic.get a in
    let (x, b) = fn old in
    if Atomic.compare_and_set a old b then x
    else gn ()
  in
  gn ()

let addr_of_sock sock =
  match Unix.getpeername sock
  with ADDR_UNIX name -> "UNIX:" ^ name
     | ADDR_INET (addr, _) -> Unix.string_of_inet_addr addr

let to_human ?(unit="o") f =
  (if f > 1e12 then Printf.sprintf "%.2fT%s" (f /. 1e12)
   else if f > 1e9 then Printf.sprintf "%.2fG%s" (f /. 1e9)
   else if f > 1e6 then Printf.sprintf "%.2fM%s" (f /. 1e6)
   else if f > 1e3 then Printf.sprintf "%.2fK%s" (f /. 1e3)
   else Printf.sprintf "%d%s" (int_of_float f)) unit

let to_human_int ?unit n = to_human ?unit (float n)

module Sfd = struct
  type t =
    { fd : Unix.file_descr
    ; closing : bool Atomic.t }

  let close { fd; closing } =
    if Atomic.compare_and_set closing false true then
      Unix.close fd

  let make fd =
    let r = { fd; closing = Atomic.make false } in
    Gc.finalise close r;
    r

  let get { fd; _ } = fd
end


let fast_concat sep ls =
  let rec size acc ls =
    match ls with
    | [] -> assert false
    | [h] -> acc + String.length h
    | h::r -> size (acc + 1 + String.length h) r
  in
  match ls with
  | [] -> ""
  | [h] -> h
  | ls ->
     let b = Bytes.create (size 0 ls) in
     let rec loop offset ls =
       match ls with
       | [] -> assert false
       | [h] ->
          Bytes.blit (Bytes.unsafe_of_string h) 0 b offset (String.length h)
       | h::ls ->
          let len = String.length h in
          Bytes.blit (Bytes.unsafe_of_string h) 0 b offset len;
          let offset = offset + len in
          Bytes.set b offset sep;
          let offset = offset + 1 in
          loop offset ls
     in
     loop 0 ls;
     Bytes.unsafe_to_string b

type file_type =
  | Inexistant
  | Reg of { fd : Unix.file_descr; mtime: float; size: int;
             mutable free: bool }
  | Dir of { fd : Unix.dir_handle; mtime: float;
             mutable free: bool }
  | Other

external file_type : string -> file_type = "caml_file_type"

let free : file_type -> unit = function
  | Reg { fd; free = true; _ } ->
     (try Unix.close fd with Unix.Unix_error _ -> ())
  | Dir { fd; free = true; _ } ->
     (try Unix.closedir fd with Unix.Unix_error _ -> ())
  | _ -> ()

let do_not_free : file_type -> unit = function
  | Reg r -> r.free <- false
  | Dir r -> r.free <- false
  | _ -> ()
