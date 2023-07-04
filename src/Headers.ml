open Response_code
include Headers_

type t = (header * string) list

exception Bad_req of Response_code.t * string * t * Cookies.t
let fail_raise ?(headers=[]) ?(cookies=[]) ~code:c fmt =
  Printf.ksprintf (fun s ->raise (Bad_req (c,s,headers,cookies))) fmt

let empty = []
let contains name headers =
  List.exists (fun (n, _) -> eq name n) headers
let get_exn ?(f=fun x->x) x h =
  snd (List.find (fun (x',_) -> eq x x') h) |> f
let get ?(f=fun x -> x) x h =
  try Some (get_exn ~f x h) with Not_found -> None
let remove x h =
  List.filter (fun (k,_) -> not (eq k x)) h
let set x y h =
  (x,y) :: remove x h
let pp out l =
  let pp_pair out (k,v) = Format.fprintf out "@[<h>%s: %s@]" (to_string k) v in
  Format.fprintf out "@[<v>%a@]" (Format.pp_print_list pp_pair) l
let set_cookies cookies h =
  List.fold_left (fun h (_, c) ->
      (Set_Cookie, Http_cookie.to_set_cookie c) :: h) h cookies

(*  token = 1*tchar
    tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." / "^" / "_"
    / "`" / "|" / "~" / DIGIT / ALPHA ; any VCHAR, except delimiters
    Reference: https://datatracker.ietf.org/doc/html/rfc7230#section-3.2 *)
let parse_ ~buf (bs:Input.t) : t * Cookies.t =
  let rec loop headers cookies =
    (try
       let k = parse bs in
       let v =
         try
           Input.read_line ~buf bs
         with _ -> fail_raise ~code:bad_request "invalid header line: %S" (to_string k)
       in
       let headers, cookies =
         if k = Cookie then
           begin
             let new_cookies = Cookies.parse v in
             (headers, List.fold_left (fun acc (_, c) ->
                           Cookies.add c acc) cookies new_cookies)
           end
         else
           ((k,v)::headers, cookies)
       in
       fun () -> loop headers cookies
     with
     | End_of_headers ->
        assert (Input.read_char bs = '\n');
        (fun () -> (headers,cookies))
     | Invalid_header s ->
        let _ = Input.read_line ~buf bs in
        Log.f (Req 3) (fun k -> k "ignoring unknown header starting with %s\n%!" s);
        (fun () -> loop headers cookies)) ()
  in
  loop [] []
