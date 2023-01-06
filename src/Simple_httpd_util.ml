
let debug_lvl = ref (
  match int_of_string (Sys.getenv "HTTP_DBG") with
  | n -> n | exception _ -> 0
)
let set_debug n = debug_lvl := n
let debug ?(lvl=1) k =
  if !debug_lvl >= lvl then (
    k (fun fmt->
       Printf.fprintf stdout "[domain %d]: " Domain.((self() :> int));
       Printf.kfprintf (fun oc -> Printf.fprintf oc "\n%!") stdout fmt)
  )

(* test utils *)
(*$inject
  let pp_res f = function Ok x -> f x | Error e -> e
  let pp_res_query = (Q.Print.(pp_res (list (pair string string))))
  let err_map f = function Ok x-> Ok (f x) | Error e -> Error e
  let sort_l l = List.sort compare l
  let eq_sorted a b = (=) (err_map sort_l a)(err_map sort_l b)
  let is_ascii_char c = Char.code c < 128
*)

let percent_encode ?(skip=fun _->false) s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | c when skip c -> Buffer.add_char buf c
      | (' ' | '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+'
        | ',' | '/' | ':' | ';' | '=' | '?' | '@' | '[' | ']' | '~')
        as c ->
        Printf.bprintf buf "%%%X" (Char.code c)
      | c when Char.code c > 127 ->
        Printf.bprintf buf "%%%X" (Char.code c)
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

(*$= & ~printer:(fun s->s)
  "hello%20world" (percent_encode "hello world")
  "%23%25^%24%40^%40" (percent_encode "#%^$@^@")
  "a%20ohm%2B5235%25%26%40%23%20---%20_" (percent_encode "a ohm+5235%&@# --- _")
*)

(*$= & ~printer:Q.(Print.(option string))
  (Some "?") (percent_decode @@ percent_encode "?")
*)

let hex_int (s:string) : int = Scanf.sscanf s "%x" (fun x->x)

let percent_decode (s:string) : _ option =
  let buf = Buffer.create (String.length s) in
  let i = ref 0 in
  try
    while !i < String.length s do
      match String.get s !i with
      | '%' ->
        if !i+2 < String.length s then (
          begin match hex_int @@ String.sub s (!i+1) 2 with
            | n -> Buffer.add_char buf (Char.chr n)
            | exception _ -> raise Exit
          end;
          i := !i + 3;
        ) else (
          raise Exit (* truncated *)
        )
      | '+' -> Buffer.add_char buf ' '; incr i (* for query strings *)
      | c -> Buffer.add_char buf c; incr i
    done;
    Some (Buffer.contents buf)
  with Exit -> None

(*$QR & ~count:1_000 ~long_factor:20
    Q.string (fun s ->
        String.iter (fun c -> Q.assume @@ is_ascii_char c) s;
        match percent_decode (percent_encode s) with
        | Some s' -> s=s'
        | None -> Q.Test.fail_report "invalid percent encoding")
*)

exception Invalid_query

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
  let i = ref 0 in
  let n = String.length s in
  while !i < n do
    match String.index_from s !i '/' with
    | exception Not_found ->
      if !i < n then (
        (* last component *)
        l := String.sub s !i (n - !i) :: !l;
      );
      i := n (* done *)
    | j ->
      if j > !i then  (
        l := String.sub s !i (j - !i) :: !l;
      );
      i := j+1;
  done;
  List.rev !l

(*$= & ~printer:Q.Print.(list string)
  ["a"; "b"] (split_on_slash "/a/b")
  ["coucou"; "lol"] (split_on_slash "/coucou/lol")
  ["a"; "b"; "c"] (split_on_slash "/a/b//c/")
  ["a"; "b"] (split_on_slash "//a/b/")
  ["a"] (split_on_slash "/a//")
  [] (split_on_slash "/")
  [] (split_on_slash "//")
*)

let parse_query s : (_ list, string) result=
  let pairs = ref [] in
  let is_sep_ = function '&' | ';' -> true | _ -> false in
  let i = ref 0 in
  let j = ref 0 in
  try
    let percent_decode s =
      match percent_decode s with Some x -> x | None -> raise Invalid_query
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
    Ok !pairs
  with
  | Invalid_argument _ | Not_found | Failure _ ->
    Error (Printf.sprintf "error in parse_query for %S: i=%d,j=%d" s !i !j)
  | Invalid_query -> Error ("invalid query string: " ^ s)

(*$= & ~printer:pp_res_query ~cmp:eq_sorted
  (Ok ["a", "b"; "c", "d"]) (parse_query "a=b&c=d")
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
        eq_sorted (Ok l) (parse_query s))
*)

let pp_date fmt date =
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
  Format.fprintf fmt "%s, %02d %s %04d %02d:%02d:%02d GMT"
    day date.tm_mday month (date.tm_year+1900) date.tm_hour date.tm_min date.tm_sec

module LinkedList = struct

  type 'a cell =
    | Nil
    | Cons of { v : 'a
              ; mutable next : 'a cell
              ; mutable prev : 'a cell }

  type 'a t = { mutable head : 'a cell
              ; mutable tail : 'a cell }

  let create () = { head = Nil; tail = Nil }

  let add_first v l =
    let cell = Cons { v; next = l.head; prev = Nil } in
    match l.head with
    | Nil -> assert (l.tail = Nil);
             l.head <- cell; l.tail <- cell
    | _   -> l.head <- cell

  let add_last v l =
    let cell = Cons { v; next = Nil; prev = l.tail } in
    match l.tail with
    | Nil -> assert (l.head = Nil);
             l.head <- cell; l.tail <- cell
    | _   -> l.tail <- cell

  type 'a prev = Cell of 'a cell | Root of 'a t

  let search_and_remove_first fn l =
    let rec gn prev = function
      | Nil -> None
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
             Some v
           end
         else
           gn (Cell cell) next
    in
    gn (Root l) l.head

  let search_and_remove_last fn l =
    let rec gn next = function
      | Nil -> None
      | Cons{ v; prev; _ } as cell ->
         if fn v then
           begin
             let cell = match next with
              | Root r -> r.tail <- prev; Nil
              | Cell (Cons c as cell) -> c.prev <- prev; cell
              | Cell Nil -> assert false
             in
             begin
               match prev with
               | Nil -> l.head <- cell
               | Cons c -> c.next <- cell
             end;
             Some v
           end
         else
           gn (Cell cell) prev
    in
    gn (Root l) l.tail
end
