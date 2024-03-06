open Compile_status_common

let _ =
  Printf.printf "type t = int\n";
  List.iter (fun h -> Printf.printf "let %s = %s\n" (to_lower h.(1)) h.(0)) fields;
  Printf.printf "\nlet descr = function";
  List.iter (fun h -> Printf.printf "  | %s -> %S\n" (to_lower h.(0))
                        h.(1)) fields;
  Printf.printf "  | _ -> failwith \"Unknown response code\"\n\n%!";
  Printf.printf "let reference = function";
  List.iter (fun h -> Printf.printf "  | %s -> %S\n" (to_lower h.(0))
                        h.(2)) fields;
  Printf.printf "  | _ -> failwith \"Unknown response code\"\n%!"
