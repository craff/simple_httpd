
let filename =
  if Array.length Sys.argv <> 2 then
    begin
      Printf.eprintf "usage: %s path_to_field_names.csv" Sys.argv.(0);
      exit 1
    end;
  Sys.argv.(1)

let ch = open_in filename

let _ = input_line ch

let rec fn acc =
  try
    let line = input_line ch in
    if String.length line > 0 && Char.lowercase_ascii line.[0] <> Char.uppercase_ascii  line.[0] then
      begin
        let line = String.split_on_char ',' line in
        let line = List.map String.trim line in
        fn (line :: acc)
      end
    else acc
  with
    End_of_file -> acc

let to_cstr = String.map (function '-' -> '_' | c -> c)

let lines = List.rev (fn [])

let fields = List.map (function [] -> assert false | (h::_) -> h) lines

let _ =
  Printf.printf "type t =\n";
  List.iter (fun h -> Printf.printf "  | %s\n" (to_cstr h)) fields;
  Printf.printf "\n%!"

let _ = Printf.printf "
val eq : t -> t -> bool
val to_string : t -> string
exception Invalid_header of string
exception End_of_headers

val parse : Simple_httpd_stream.t -> t
"
