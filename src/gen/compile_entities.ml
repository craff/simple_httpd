
let filename =
  if Array.length Sys.argv <> 2 then
    begin
      Printf.eprintf "usage: %s path_to_field_names.csv" Sys.argv.(0);
      exit 1
    end;
  Sys.argv.(1)

let json = Yojson.Basic.from_file filename

let _ =
  Printf.printf "let tbl = Hashtbl.create 1024\n";
  match json with
  | `Assoc l ->
     let fn (key, v) = match v with
         `Assoc l -> let codes = List.assoc "codepoints" l in
                     let l = match codes with
                       | `List l ->
                          List.map (function `Int x -> x
                                           | _ -> assert false) l
                       | _ -> assert false
                     in
                     let prl ch = List.iter (Printf.fprintf ch "%d;") in
                     Printf.printf "let _ = Hashtbl.add tbl %S [%a]\n" key prl l
       | _ -> assert false
     in List.iter fn l
  | _ -> assert false
