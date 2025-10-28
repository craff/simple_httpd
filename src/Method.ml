open Response_code

type t =
  | GET
  | PUT
  | POST
  | HEAD
  | DELETE
  | OPTIONS
  | CONNECT
  | TRACE

let to_string = function
  | GET -> "GET"
  | PUT -> "PUT"
  | HEAD -> "HEAD"
  | POST -> "POST"
  | DELETE -> "DELETE"
  | OPTIONS -> "OPTIONS"
  | CONNECT -> "CONNECT"
  | TRACE -> "TRACE"

let pp out s = Format.pp_print_string out (to_string s)

let of_string = function
  | "GET" -> GET
  | "PUT" -> PUT
  | "POST" -> POST
  | "HEAD" -> HEAD
  | "DELETE" -> DELETE
  | "OPTIONS" -> OPTIONS
  | "CONNECT" -> CONNECT
  | "TRACE" -> TRACE
  | s -> Headers.fail_raise ~code:bad_request "unknown method %S" s

let parse input =
  let open Input in
  branch_char (function
      | 'G' -> exact_string "ET" GET
      | 'P' -> branch_char (function
                   | 'U' -> exact_char 'T' PUT
                   | 'O' -> exact_string "ST" POST
                   | _   -> fail_parse input)
      | 'H' -> exact_string "EAD" HEAD
      | 'D' -> exact_string "ELETE" DELETE
      | 'O' -> exact_string "PTIONS" OPTIONS
      | 'C' -> exact_string "ONNECT" CONNECT
      | 'T' -> exact_string "RACE" TRACE
      | _   -> fail_parse input) input
