type t =
  | GET
  | PUT
  | POST
  | HEAD
  | DELETE

let to_string = function
  | GET -> "GET"
  | PUT -> "PUT"
  | HEAD -> "HEAD"
  | POST -> "POST"
  | DELETE -> "DELETE"
let pp out s = Format.pp_print_string out (to_string s)

let of_string = function
  | "GET" -> GET
  | "PUT" -> PUT
  | "POST" -> POST
  | "HEAD" -> HEAD
  | "DELETE" -> DELETE
  | s -> Response_code.bad_reqf 400 "unknown method %S" s
