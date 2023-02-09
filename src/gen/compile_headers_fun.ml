
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
  Printf.printf "type header =\n";
  List.iter (fun h -> Printf.printf "  | %s\n" (to_cstr h)) fields;
  Printf.printf "\n%!"

type tree = { leaf : string option; nodes : (char * (char * tree)) list; id : int }

let count = ref 0

let empty () =
  let id = !count in
  incr count;
  { leaf = None; nodes = []; id}

let rec sort tree =
  let nodes = List.sort (fun (c1,_) (c2,_) -> compare c1 c2) tree.nodes in
  let nodes = List.map (fun (c,(d,s)) -> (c,(d,sort s))) nodes in
  { tree with nodes }

let tree : tree =
  let rec fn tree h0 h =
    let len = String.length h in
    if len = 0 then { tree with leaf = Some h0 } else
    let c = Char.uppercase_ascii h.[0] in
    let h' = String.sub h 1 (len - 1) in
    let son = try snd (List.assoc c tree.nodes)
              with Not_found -> empty ()
    in
    let nodes = List.filter (fun (c',_) -> c <> c') tree.nodes in
    let son = fn son h0 h' in
    { tree with nodes = (c,(h.[0], son)) :: nodes }
  in
  let rec gn tree = function
    | [] -> tree
    | h :: l -> gn (fn tree h h) l
  in
  sort (gn (empty ()) fields)

let _ = Printf.printf "
let eq (h1:header) (h2:header) = h1 = h2
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
exception Found of header

type fn = Cell of (char -> fn) [@unboxed]
|}

let both c1 =
  let c2 = Char.lowercase_ascii c1 in
  if c1 = c2 then Printf.sprintf "%C" c1 else
    Printf.sprintf "%C|%C" c1 c2

let rec fn first tree k =
  List.iter (fun (_,(c0,t)) ->
      let k = k ^ String.make 1 c0 in
      fn false t k) tree.nodes;
  let cases = List.map (fun (c,(_,t)) ->
                  Printf.sprintf "    | %s -> cell_%d" (both c) t.id) tree.nodes
  in
  let default = Printf.sprintf "    | c -> raise (Invalid_header (%S ^ String.make 1 c))" k in
  let cases = String.concat "\n" (cases @ [default]) in
  Printf.printf "let cell_%d = Cell (fun c -> %smatch c with ':' -> %s\n%s)\n"
  tree.id
  (if first then "if c = '\\r' then raise End_of_headers;\n"
            else "")
  (match tree.leaf with
   | None -> Printf.sprintf "    raise (Invalid_header %S)" k
   | Some h -> Printf.sprintf "    raise (Found %s)" (to_cstr h)) cases

let _ = fn true tree ""

let _ = Printf.printf "%s\n%!"
"let parse self =
  let open Simple_httpd_input in
  let acc = ref cell_0 in
  try
    while true do
      for j = self.off to self.off + self.len - 1 do
        self.consume 1;
        let Cell f = !acc in
        acc := f (Bytes.get self.bs j);
      done;
      self.fill_buf();
      if self.len = 0 then raise End_of_file
    done;
    assert false
  with Found k -> k"
