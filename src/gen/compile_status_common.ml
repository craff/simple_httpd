
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
    if String.length line > 0 then
      begin
        let line = String.split_on_char ',' line in
        let line = List.map String.trim line in
        fn (line :: acc)
      end
    else acc
  with
    End_of_file -> acc

let to_cstr = String.map (function ('-'|' ') -> '_' | c -> c)
let to_lower s = String.lowercase_ascii (to_cstr s)

let lines = List.rev (fn [])
let lines = List.filter (function _::x::_ -> x <> "Unassigned"
                         && String.index_opt x '(' = None
                        | _ -> false) lines
let lines = List.map (function x::y::l -> x::y::String.concat " " l::[]
                              | _ -> assert false) lines
let fields = List.map Array.of_list lines
