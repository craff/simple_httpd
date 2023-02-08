
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

type tree = { leaf : string option; nodes : (char * (char * tree)) list }
let empty = { leaf = None; nodes = [] }

let rec sort tree =
  let nodes = List.sort (fun (c1,_) (c2,_) -> compare c1 c2) tree.nodes in
  let nodes = List.map (fun (c,(d,s)) -> (c,(d,sort s))) nodes in
  { tree with nodes }

let tree : tree =
  let rec fn tree h0 h =
    let len = String.length h in
    if len = 0 then
      { tree with leaf = Some h0 }
    else
      let c = Char.uppercase_ascii h.[0] in
      let h' = String.sub h 1 (len - 1) in
      let son = try snd (List.assoc c tree.nodes)
                with Not_found -> empty
      in
      let nodes = List.filter (fun (c',_) -> c <> c') tree.nodes in
      let son = fn son h0 h' in
      { tree with nodes = (c,(h.[0], son)) :: nodes }
  in
  let rec gn tree = function
    | [] -> tree
    | h :: l -> gn (fn tree h h) l
  in
  sort (gn empty fields)

let _ = Printf.printf "
let eq (h1:t) (h2:t) = h1 = h2
"

let _ =
  Printf.printf {|
let to_string = function
|};
  List.iter (fun h -> Printf.printf "  | %s -> %S\n" (to_cstr h) h) fields;
  Printf.printf "\n%!"

let _ = Printf.printf "%s" {|
exception Invalid_header of string
exception End_of_headers

let parse ~buf is =
  let getc () = Simple_httpd_stream.read_char is in
  let rec finish_loop () =
    let c = getc () in
    if c = ':' then raise (Invalid_header (Buffer.contents buf))
    else finish_loop ()
  in
  let finish k c =
    Buffer.clear buf;
    Buffer.add_string buf k;
    Buffer.add_char buf c;
    finish_loop ()
  in

|}

let both c1 =
  let c2 = Char.lowercase_ascii c1 in
  if c1 = c2 then Printf.sprintf "%C" c1 else
    Printf.sprintf "%C|%C" c1 c2

let rec fn first tree k =
  let cases = List.map (fun (c,(c0,t)) ->
                  let k = k ^ String.make 1 c0 in
                  Printf.sprintf "    | %s -> (%s)" (both c) (fn false t k)) tree.nodes
  in
  let default = Printf.sprintf "    | c -> finish %S c" k in
  let cases = String.concat "\n" (cases @ [default]) in
  Printf.sprintf "
  let c = getc () in %s
  match c with ':' -> %s\n%s
  "
  (if first then "\nif c = '\\r' then (assert (getc () = '\\n'); raise End_of_headers);"
            else "")
  (match tree.leaf with
   | None -> Printf.sprintf "    raise (Invalid_header %S)" k
   | Some h -> Printf.sprintf "    %s" (to_cstr h)) cases

let _ = Printf.printf "%s\n%!" (fn true tree "")
