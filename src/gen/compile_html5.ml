
let load filename =
  let ch = open_in filename in

  let _ = input_line ch in

  let rec fn acc =
    try
      let line = input_line ch in
      if String.length line > 0 then
        begin
          let line = String.split_on_char '|' line in
          let line = List.map String.trim line in
          fn (line :: acc)
        end
      else acc
    with
      End_of_file -> acc
  in
  List.rev (fn [])

let ml = open_out "./Html.ml"
let mli = open_out "./Html.mli"

let remove_star v =
  if String.length v > 0 && v.[String.length v - 1] = '*'
  then String.sub v 0 (String.length v - 1)
  else v

let string_list line n =
  try
    let v = List.nth line n in
    let v = List.flatten (List.map (String.split_on_char ',')
                            (String.split_on_char ';' v)) in
    List.map remove_star v
  with Failure "nth" [@warning "-52"] -> []

let to_upper str =
  String.mapi (fun i c -> if i = 0 then Char.uppercase_ascii c
               else if c = '-' then '_' else c) str

let normalize c =
  let c = match c with
    | "sectioningroot" -> "sectioningRoot"
    | "form-associated" -> "formAssociated"
    | _ -> c
  in
  to_upper c

let do_tag line =
  let tag = List.nth line 1 in
  let descr = try List.nth line 2 with Failure "nth" [@warning "-52"] -> "" in
  let cat = List.map normalize (string_list line 3) in
  let parents = List.map normalize (string_list line 4) in
  let children = List.map normalize (string_list line 5) in
  let attributes = string_list line 6 in
  let interface = try List.nth line 7 with _ -> "" in
  (tag, descr, cat, parents, children, attributes, interface)

let do_attr line =
  let attr = List.nth line 1 in
  let elems = List.map normalize (string_list line 2) in
  let descr = try List.nth line 3 with Failure "nth" [@warning "-52"] -> "" in
  let value = List.map normalize (string_list line 4) in
  (attr, elems, descr, value)

let tags = List.map do_tag (load "table-1.csv")

let tags =
  List.flatten (
      List.map (
          fun (tag, d, c, p, ch, a, i) ->
                let tags = String.split_on_char ',' tag in
                List.map (fun x -> (x, d, c, p, ch, a, i)) tags) tags)

let attrs = List.map do_attr (load "table-3.csv")
            @ List.map do_attr (load "table-4.csv")

let tag_type ch =
  Printf.fprintf ch "type tag = Foreign of string | Main | Text | Template | Picture\n";
  let fn (tag, _descr, _cat, _parents, _children, _attributes, _interface) =
    Printf.fprintf ch "  | %s\n" (to_upper tag);
  in
  List.iter fn tags;
  Printf.fprintf ch "\n"

let _ = tag_type ml; tag_type mli

let tag_to_string ch =
  Printf.fprintf ch "let tag_to_string = function\n";
  let fn (tag, _descr, _cat, _parents, _children, _attributes, _interface) =
    Printf.fprintf ch "  | %s -> %S\n" (to_upper tag) tag;
  in
  List.iter fn tags;
  Printf.fprintf ch "  | Main -> \"main\" | Text -> \"text\" | Template -> \"template\" | Picture -> \"picture\" | Foreign s -> s\n\n"

let _ = tag_to_string ml; Printf.fprintf mli "val tag_to_string : tag -> string\n"

let tag_of_string ch =
  Printf.fprintf ch "let tag_of_string s = match String.lowercase_ascii s with\n";
  let fn (tag, _descr, _cat, _parents, _children, _attributes, _interface) =
    Printf.fprintf ch "  | %S -> %s\n" tag (to_upper tag);
  in
  List.iter fn tags;
  Printf.fprintf ch "  | \"template\" -> Template | \"main\" -> Main | \"picture\" -> Picture | s -> Foreign s\n\n"

let _ = tag_of_string ml; Printf.fprintf mli "val tag_of_string : string -> tag\n"

let attr_table = Hashtbl.create 101

let attr_type ch =
  let adone = ref [] in
  let add_tag_set attr l =
    let old = try Hashtbl.find  attr_table attr with Not_found -> Some [] in
    let new_ = match l, old with
      | ["HTMLelements"], _ -> None
      | _, None -> None
      | l, Some l' ->
         Some (List.fold_left (fun l x ->
                   let x = to_upper x in
                   if List.mem x l then l else x::l) l' l)
    in
    Hashtbl.replace attr_table attr new_
  in
  Printf.fprintf ch "type attr = ForeignAttr of string\n";
  let fn (attr, elems, _descr, _value) =
    if not (List.mem attr !adone) then
      begin
        adone := attr :: !adone;
        Printf.fprintf ch "  | %s\n" (to_upper attr);
      end;
    add_tag_set attr elems;
  in
  List.iter fn attrs;
  Printf.fprintf ch "\n"

let _ = attr_type ml; attr_type mli

let attr_to_string ch =
  Printf.fprintf ch "let attr_to_string = function\n";
  let adone = ref [] in
  let fn (attr, _, _, _) =
    if not (List.mem attr !adone) then
      begin
        adone := attr :: !adone;
        Printf.fprintf ch "  | %s -> %S\n" (to_upper attr) attr;
      end
  in
  List.iter fn attrs;
  Printf.fprintf ch "  | ForeignAttr s -> s\n\n"

let _ = attr_to_string ml; Printf.fprintf mli "val attr_to_string : attr -> string\n"

let attr_of_string ch =
  Printf.fprintf ch "let attr_of_string s = match String.lowercase_ascii s with\n";
  let adone = ref [] in
  let fn (attr, _, _, _) =
    if not (List.mem attr !adone) then
      begin
        adone := attr :: !adone;
        Printf.fprintf ch "  | %S -> %s\n" attr (to_upper attr);
      end
  in
  List.iter fn attrs;
  Printf.fprintf ch "  | s -> ForeignAttr s\n\n"

let _ = attr_of_string ml; Printf.fprintf mli "val attr_of_string : string -> attr\n"

let cats =
  let cats = ref [] in
  let fn (_tag, _descr, cat, _parents, _children, _attributes, _interface) =
    List.iter (fun c ->
        if c <> "" && not (List.mem c !cats) then cats := c :: !cats) cat
  in
  List.iter fn tags;
  List.sort compare !cats

let categorie_type ch =
  Printf.fprintf ch "type categorie = \n";
  let fn c =
    Printf.fprintf ch "  | %s\n" (to_upper c);
  in
  List.iter fn cats;
  Printf.fprintf ch "\n"

let _ = categorie_type ml; categorie_type mli

let cat_to_string ch =
  Printf.fprintf ch "let cat_to_string = function\n";
  let fn cat =
    Printf.fprintf ch "  | %s -> %S\n" (to_upper cat) (to_upper cat);
  in
  List.iter fn cats

let _ = cat_to_string ml; Printf.fprintf mli "val cat_to_string : categorie -> string\n"

let cat_of_string ch =
  Printf.fprintf ch "exception Unknown of string\n";
  Printf.fprintf ch "let cat_of_string s = match s with\n";
  let fn cat =
    Printf.fprintf ch "  | %S -> %s\n" (to_upper cat) (to_upper cat);
  in
  List.iter fn cats;
  Printf.fprintf ch "  | s -> raise (Unknown s)\n\n"

let _ =
  cat_of_string ml;
  Printf.fprintf mli "exception Unknown of string\n";
  Printf.fprintf mli "val cat_of_string : string -> categorie\n"

let info ch =
  Printf.fprintf ch "\
type children
type parent
type _ relation =
    | Cat : categorie -> 'a relation
    | Tag : tag -> 'a relation
    | Transparent : children relation
    | Data : children relation
type tag_info = { categories : categorie list
            ; parents : parent relation list
            ; children : children relation list }\n
type tags = Any | Some of tag list\n";
  if ch == ml then
    begin
      Printf.fprintf ch "let info_tbl = Hashtbl.create 101\n";
      Printf.fprintf ch "let attr_tbl = Hashtbl.create 101\n\n"
    end
  else
    begin
      Printf.fprintf ch "val info_tbl : (tag, tag_info) Hashtbl.t\n";
      Printf.fprintf ch "val attr_tbl : (attr, tags) Hashtbl.t\n\n"
    end
let _ = info ml; info mli

let raw_element = [ "script"; "style"; "textarea"; "title" ]

let info_tbl ch =
  let fn (tag, _descr, cat, parents, children, _attributes, _interface) =
    let pp el par ch l =
      let l = if l = ["Metadatacontent"] then
                ["Base";"Command";"Link";"Meta";"Noscript";"Script";"Style";"Title"]
              else if l = ["Empty"] then [] else l
      in
      List.iter (fun cat_el ->
          if cat_el <> "" then
            begin
              if List.mem cat_el cats then
                if el then Printf.fprintf ch "Cat(%s);" cat_el
                else Printf.fprintf ch "%s;" cat_el
              else if cat_el = "Transparent" || tag = "noscript" then
                if el && not par then Printf.fprintf ch "Transparent;"
                else ()
              else if List.mem tag raw_element then
                if el && not par then Printf.fprintf ch "Data;"
                else ()
              else
                if el then Printf.fprintf ch "Tag(%s);" cat_el
                else (Printf.eprintf "===> %S\n%!" cat_el; assert false)
            end) l
    in
    Printf.fprintf ch
      "let _ = Hashtbl.add info_tbl %s { categories = [%a]; parents = [%a]; children = [%a]}\n"
      (to_upper tag) (pp false false) cat (pp true true)
      parents (pp true false) children;
  in
  List.iter fn tags;
  (* template using the same rule as script CHECK for attribute ?*)
  Printf.fprintf ch "let _ = Hashtbl.add info_tbl Template (Hashtbl.find info_tbl Script)\n";
  (* picture *)
  Printf.fprintf ch "let _ = Hashtbl.add info_tbl Picture
                     { (Hashtbl.find info_tbl Video) with children = [Tag(Source); Tag(Img)] }\n";
  (* main *)
  Printf.fprintf ch "let _ = Hashtbl.add info_tbl Main (Hashtbl.find info_tbl Div)\n\n"

let _ = info_tbl ml

let attr_tbl ch =
  let pa ch = function
    | None -> Printf.fprintf ch "Any"
    | Some ["Border"] -> Printf.fprintf ch "Any"  (* CHECK: phrasing ? *)
    | Some l -> Printf.fprintf ch "(Some[%a])" (fun ch l ->
                    List.iter (fun x ->
                        Printf.fprintf ch "%s;" x) l) l
  in
  let fn attr tags =
    Printf.fprintf ch "let _ = Hashtbl.add attr_tbl %s %a\n"
      (to_upper attr) pa tags  in
  Hashtbl.iter fn attr_table;
  Printf.fprintf ch "\n"

let _ = attr_tbl ml

let _ = close_out ml; close_out mli
