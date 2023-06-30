open Markup

let name_to_string = function
  | "", local_name -> local_name
  | _, local_name -> local_name

let print_tag name attributes =
  let attributes =
    attributes
    |> List.map (fun (name, value) ->
           let len = String.length value in
           let value =
             if len > 0 && value.[0] = '?' then
               let s = String.sub value 1 (len - 1) in
               "\" ^ (" ^ s ^ ") ^ \""
             else value
           in
           Printf.sprintf " %s=\"%s\"" (name_to_string name) value)
    |> String.concat ""
  in
  Printf.sprintf "<%s%s>" (name_to_string name) attributes

let print_closing name =
  Printf.sprintf "</%s>" (name_to_string name)

type attrs = (name * string) list
type tree_c =
  | Doctype of doctype
  | Element of name * attrs * tree list
  | Caml of attrs * tree list (* TODO: will we use attributes ? *)
  | Text of string list
  | Comment of string
  | PI of (string * string)
  | Xml of xml_declaration
  | End_element of name

and tree = { loc : location; c : tree_c }

let noloc c = { loc = (-1,-1); c }

let doctype ~loc d = {loc; c = Doctype(d) }
let element ~loc name attrs children = {loc; c =
  if snd name = "ml" then
    Caml(attrs,children)
  else
    Element(name,attrs,children)}

let text ~loc l = {loc; c = Text(l)}
let comment ~loc c = {loc; c = Comment(c)}
let pi ~loc s1 s2 = {loc; c = PI(s1,s2)}
let xml ~loc x = {loc; c = Xml(x)}

let parse_html s =
  s |> parse_html |>
    trees_with_loc ~element ~text ~comment ~pi ~xml ~doctype

let trees_to_ocaml ~filename t =
  let buf = Buffer.create 4096 in
  let filter = ref None in
  let doctype = ref false in
  let pr fmt = Printf.bprintf buf fmt in
  let of_elems top acc =
    let fn = function
      | `End_element(name) -> print_closing name
      | `Start_element(name,attrs) ->
         print_tag name attrs
      | (`Comment _ | `Doctype _ | `PI _ | `Text _ | `Xml _) as s  ->
         let r = ref (Some s) in
         let fn () = let x = !r in r := None; x in
         stream fn |> write_html |> to_string
    in
    if acc <> [] then
      begin
        let s = String.trim (String.concat "" (List.map fn (List.rev acc))) in
        if top then
          (if s <> "" then pr "let _ = echo %S;;" s)
        else
          pr "%S" s
      end
  in
  let rec fn depth acc stack =
    let top = depth = 0 in
    let newline depth =
      if depth = 0 then pr "\n";
      let rec fn d =
        if d > 0 then (pr"  "; fn (d - 1))
      in
      fn depth
    in
    let gn s stack = fn depth (s::acc) stack in
    match stack with
    | [] -> if top then acc else (of_elems top acc; [])
    | e::stack ->
       match e.c with
       | Caml(_a, sons) ->
          if top && not !doctype && !filter = None
            && acc <> [] then failwith "chaml: nothing allowed before ML filter";
          of_elems top acc;
          let hn i x =
            newline (if i = 0 then depth else depth + 1);
            match x.c with
            | Text l -> pr "\n#%d %S\n" (fst x.loc) filename; List.iter (pr "%s") l
            | _      -> of_elems false (fn (depth + 1) [] [x]);
          in
          List.iteri hn sons;
          if top && not !doctype && !filter = None then
            begin
              filter := Some (Buffer.contents buf);
              Buffer.clear buf
            end
          else if top then
            begin
              pr " ;;";
            end;
          newline depth;
          fn depth [] stack
       | Element (n,a,sons) ->
          gn (`Start_element(n,a)) (sons @ noloc (End_element n) :: stack)
       | End_element(name) -> gn (`End_element(name)) stack
       | Doctype d -> doctype := true; gn (`Doctype d) stack
       | Text l -> gn (`Text l) stack
       | Comment c -> gn (`Comment c) stack
       | PI (s,u) -> gn (`PI (s,u)) stack
       | Xml x -> gn (`Xml x) stack
  in
  of_elems true (fold (fun acc x -> fn 0 acc [x]) [] t);
  (Buffer.contents buf, !filter)

(*
let test =
  {html|<!DOCTYPE html><html><head><title>coucou</title></head>
   <ML>let tmp = "background: black;"</ML>
   <body><ML>let x = <h1 style="?tmp">coucou, c'est l'été<ML>^ test ^</ML></h1></ML><ML>ouput_string x</ML></body>|html}

let t = string test |> parse_html |> trees_to_ocaml
 *)
