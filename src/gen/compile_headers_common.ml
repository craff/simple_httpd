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

let extra = [
  ["CF-Connecting-IP"; "nonstandard"; "";
   "Added by Proxy, list of addresses"];
  ["Filename-Multipart"; "nonstandard"; "";
   "Managment of filenames in multipart encoded data as a fake header"; ""];
  ["X-CSRF-Token"; "nonstandard"; "";
   "Added by Proxy, list of addresses"];
  ["X-Forwarded-For"; "nonstandard"; "";
   "Added by Proxy, list of addresses"];
  ["X-Real-IP"; "nonstandard"; "";
   "Added by Proxy, original addresses"]]


let lines = List.rev (fn extra)

type status =
  Permanent
| Provisional
| Deprecated
| Obsoleted
| NonStandard

type typ =
  List
| Dictionnary
| Item
| NoType

let string_to_type h = function
    "List" -> List
  | "Dictionary" -> Dictionnary
  | "Item" -> Item
  | "" -> NoType
  | other ->
     Printf.eprintf "unsupported type for %s: %s\n%!" h other;
     assert false

let string_to_status h = function
    "permanent" -> Permanent
  | "provisional" -> Provisional
  | "deprecated" -> Deprecated
  | "obsoleted" -> Obsoleted
  | "nonstandard" -> NonStandard
  | other ->
     Printf.eprintf "unsupported status for %s: %s\n%!" h other;
     assert false

let fields =
  List.filter_map
    (function
     | (h::s::t::_) ->
        let s = string_to_status h s in
        let t = string_to_type h t in
        if s = Obsoleted then None else Some(h,s,t)
     | _ -> assert false) lines
