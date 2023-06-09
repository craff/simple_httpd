open Http_cookie
exception BadCookies of string
type nonrec t = (string * t) list

let empty = []

let parse s =
  match of_cookie s with
  | Ok l ->
     List.fold_left (fun acc c -> (name c, c)::acc) [] l
  | Error err -> raise (BadCookies err)

let add name c cookies =
  let cookies = List.filter (fun (n, _) -> n <> name) cookies in
  (name, c) :: cookies

let create : ?path:string ->
             ?domain:string ->
             ?expires:date_time ->
             ?max_age:int64 ->
             ?secure:bool ->
             ?http_only:bool ->
             ?same_site:same_site ->
             ?extension:string ->
             name:string ->
             string -> t -> t =
  fun ?path ?domain ?expires ?max_age ?secure ?http_only
      ?same_site ?extension ~name value cookies ->
  match create ?path ?domain ?expires ?max_age ?secure ?http_only
          ?same_site ?extension ~name value
  with Ok c -> add name c cookies
     | Error err -> raise (BadCookies err)

let get name cookies =
  List.assoc name cookies

let delete_all cookies =
  List.map (fun (name, c) -> (name, Http_cookie.expire c)) cookies

let delete name cookies =
  try
    let c = List.assoc name cookies in
    let cookies = List.filter (fun (n,_) -> n <> name) cookies in
    (name, Http_cookie.expire c) :: cookies
  with
    Not_found -> cookies
