open Compile_headers_common

let _ =
  Printf.printf "type header =\n";
  List.iter (fun (h,_) -> Printf.printf "  | %s\n" (to_cstr h)) fields;
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
    | (h, _) :: l -> gn (fn tree h h) l
  in
  sort (gn (empty ()) fields)

let _ = Printf.printf "
let eq (h1:header) (h2:header) = h1 = h2
"

let _ =
  Printf.printf {|
let to_string = function
|};
  List.iter (fun (h,_) -> Printf.printf "  | %s -> %S\n" (to_cstr h) h) fields;
  Printf.printf "\n%!"

let _ = Printf.printf "%s" {|
exception Invalid_header of string
exception End_of_headers
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
                  Printf.sprintf "    | %s -> fn_%d _input" (both c) t.id) tree.nodes
  in
  let default = Printf.sprintf "    | c -> raise (Invalid_header (%S ^ String.make 1 c))" k in
  let cases = String.concat "\n" (cases @ [default]) in
  Printf.printf "let fn_%d = Input.branch_char (fun c _input -> %smatch c with %s\n%s)\n"
  tree.id
  (if first then "if c = '\\r' then raise End_of_headers;\n"
            else "")
  (match tree.leaf with
   | None -> ""
   | Some h -> Printf.sprintf "  ':' -> %s" (to_cstr h)) cases

let _ = fn true tree ""

let _ = Printf.printf "let parse = fn_0\n%!"
