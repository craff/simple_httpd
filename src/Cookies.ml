open Http_cookie
exception BadCookies of string
type nonrec t = t list

let empty = []

let parse s =
  match of_cookie s with
  | Ok l -> l
  | Error err -> raise (BadCookies err)

let add c cookies =
  let name = name c in
  let cookies = List.filter (fun c -> Http_cookie.name c  <> name) cookies in
  c :: cookies

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
  with Ok c -> add c cookies
     | Error err -> raise (BadCookies err)

let get name cookies =
  List.find (fun c -> Http_cookie.name c = name) cookies

let delete_all cookies =
  List.map Http_cookie.expire cookies

let delete name cookies =
  try
    let c = get name cookies in
    let cookies = List.filter (fun c -> Http_cookie.name c <> name) cookies in
    Http_cookie.expire c :: cookies
  with
    Not_found -> cookies
