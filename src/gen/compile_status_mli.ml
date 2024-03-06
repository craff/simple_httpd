open Compile_status_common

let _ =
  Printf.printf "type t = private int\n";
  Printf.printf "(** type of status code*)\n\n";
  Printf.printf "val descr : t -> string\n";
  Printf.printf "(** description of the status code*)\n\n";
  Printf.printf "val reference : t -> string\n";
  Printf.printf "(** reference to the status code*)\n\n";

  Printf.printf "(** value of all codes registered*)\n\n";
  List.iter (fun h -> Printf.printf "val %s : t\n" (to_lower h.(1))) fields;
  Printf.printf "%!";
