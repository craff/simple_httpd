
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
let eq (h1:t) (h2:t) = h1 = h2
"

let _ =
  Printf.printf {|
let to_string = function
|};
  List.iter (fun h -> Printf.printf "  | %s -> %S\n" (to_cstr h) h) fields;
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
    else (
      let c = Char.uppercase_ascii h.[0] in
      let h' = String.sub h 1 (len - 1) in
      let son = try snd (List.assoc c tree.nodes)
                with Not_found -> empty
      in
      let nodes = List.filter (fun (c',_) -> c <> c') tree.nodes in
      let son = fn son h0 h' in
      { tree with nodes = (c,(h.[0], son)) :: nodes })
  in
  let rec gn tree = function
    | [] -> tree
    | h :: l -> gn (fn tree h h) l
  in
  sort (gn empty fields)

type tree2 = { leaf2 : string option; nodes2 : tree2 option array;
               id2 : int; offset2: int; acc2 : string }

let cid = ref 0

let rec compile_nodes acc l =
  if l = [] then (0, [||]) else
  let c0 = match l with [] -> assert false
                      | (c,_)::_ -> c in
  let c1 = match List.rev l with [] -> assert false
                               | (c,_)::_ -> c in
  let l = ref l in
  let offset = Char.code c0 in
  let size   = Char.code c1 - offset + 1 in
  (offset, Array.init size (fun i ->
      match !l with
      | [] -> assert false
      | (c,(d,t)) :: r ->
         if Char.code c - offset = i then (
           l := r;
           Some (compile_tree (acc ^ String.make 1 d) t))
         else
           None))

and compile_tree acc2 t =
  let id2 = !cid in incr cid;
  let (offset2, nodes2) = compile_nodes acc2 t.nodes in
  { id2; leaf2 = t.leaf; nodes2; acc2; offset2}

let ctree = compile_tree "" tree

let rec output_nodes r =
  Array.iter (function
      | None -> ()
      | Some r -> output_nodes r) r.nodes2;
  Printf.printf "let leaf_%d = %s\n"
    r.id2 (match r.leaf2 with None -> Printf.sprintf "Bad %S" r.acc2
                            | Some c -> Printf.sprintf "Good %s" (to_cstr c));
  Printf.printf "let offset_%d = %d\n"
    r.id2 r.offset2;
  Printf.printf "let tbl_%d = [|%s|]\n" r.id2
    (String.concat ";" (Array.to_list
       (Array.mapi (fun i -> function
            | None -> Printf.sprintf "{leaf=Bad %S;tbl=[||];offset=0}" (r.acc2 ^ String.make 1 (Char.chr (i + r.offset2)))
            | Some r -> Printf.sprintf "{leaf=leaf_%d;tbl=tbl_%d;offset=offset_%d}" r.id2 r.id2 r.id2)
          r.nodes2)))

let _ =
  Printf.printf "type leaf = Good of t | Bad of string\n
                 type cell = { leaf : leaf; tbl : cell array; offset: int }\n";
  output_nodes ctree

  let _ = Printf.printf "%s" {|
exception Invalid_header of string
exception End_of_headers

let parse ~buf is =
  let getc () = Simple_httpd_stream.read_char is in
  let rec finish_loop () =
    let c = getc () in
    if c = ':' then raise (Invalid_header (Buffer.contents buf))
    else (Buffer.add_char buf c;
          finish_loop ())
  in
  let finish str c =
    Buffer.clear buf;
    Buffer.add_string buf str;
    Buffer.add_char buf c;
    finish_loop ()
  in
  let rec fn leaf tbl offset c =
    if c = ':' then (match leaf with Good h -> h
                     | Bad s -> raise (Invalid_header s)) else
    let i = Char.code (Char.uppercase_ascii c) - offset in
    if i >= 0 && i < Array.length tbl then
      (let c = getc () in
       let {leaf;tbl;offset} = tbl.(i) in fn leaf tbl offset c)
    else
      finish (match leaf with Good h -> to_string h
                            | Bad s -> s) c
  in
  let c = getc () in
  if c = '\r' then (assert (getc () = '\n'); raise End_of_headers);
  fn leaf_0 tbl_0 offset_0 c
|}
