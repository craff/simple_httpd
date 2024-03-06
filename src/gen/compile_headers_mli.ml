open Compile_headers_common

let _ =
  Printf.printf "type header =\n";
  List.iter (fun h -> Printf.printf "  | %s\n" (to_cstr h)) fields;
  Printf.printf "\n%!"

let _ = Printf.printf "
val eq : header -> header -> bool
val to_string : header -> string
exception Invalid_header of string
exception End_of_headers

val parse : Input.t -> header
"
