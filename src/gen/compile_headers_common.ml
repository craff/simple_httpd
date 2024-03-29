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

let filename = ["Filename-Multipart"; ""; "provisional"; "Managment of filenames in multipart encoded data as a fake header"; ""]

let lines = List.rev (fn [filename])

type status =
  Permanent
| Provisional
| Deprecated
| Obsoleted

let string_to_status = function
    "permanent" -> Permanent
  | "provisional" -> Provisional
  | "deprecated" -> Deprecated
  | "obsoleted" -> Obsoleted
  | _ -> assert false

let fields =
  List.filter_map
    (function
     | (h::_::s::_) ->
        let s = string_to_status s in
        if s = Obsoleted then None else Some(h,s)
     | _ -> assert false) lines
