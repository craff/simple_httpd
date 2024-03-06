open Compile_headers_common

let _ =
  Printf.printf "type header =\n";
  List.iter (fun (h, s) ->
      Printf.printf "  | %s\n" (to_cstr h);
      if s = Deprecated then
        Printf.printf "[@ocaml.deprecated \"Header %s is deprecated\"]" h
    ) fields;
  Printf.printf "\n%!"

let _ = Printf.printf "
val eq : header -> header -> bool
val to_string : header -> string
exception Invalid_header of string
exception End_of_headers

val parse : Input.t -> header
"
