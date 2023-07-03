open Markup

let name_to_string = function
  | "", local_name -> local_name
  | _, local_name -> local_name

let print_tag name attributes =
  let args = ref [] in
  let attributes =
    attributes
    |> List.map (fun (name, value) ->
           let len = String.length value in
           let value =
             if len > 0 && value.[0] = '$' then
               let s = String.sub value 1 (len - 1) in
               args := s :: !args;
               "%S"
             else if len > 1 && value.[0] = '\\' && value.[1] = '$' then
               String.sub value 1 (len - 1)
             else value
           in
           Printf.sprintf " %s=%s" (snd name) value)
    |> String.concat ""
  in
  (Printf.sprintf "<%s%s>" (name_to_string name) attributes, List.rev !args)

let print_closing name =
  Printf.sprintf "</%s>" (name_to_string name)

type attrs = (name * string) list
type tree_c =
  | Doctype of doctype
  | Element of name * attrs * tree list
  | Caml of attrs * tree list (* TODO: will we use attributes ? *)
  | Include of string
  | Text of string list
  | Comment of string
  | PI of (string * string)
  | Xml of xml_declaration
  | End_element of name

and tree = { loc : location; c : tree_c }

let noloc c = { loc = (-1,-1); c }

let doctype ~loc d = {loc; c = Doctype(d) }
let element ~filename ~loc name attrs children =
  {loc; c =
  if String.lowercase_ascii (snd name) = "ml" then
    Caml(attrs,children)
  else if String.lowercase_ascii (snd name) = "include" then
    begin
      match children with
      | [{c = Text [s]; _}] -> Include s
      | _ -> failwith (Printf.sprintf "bad include at %s line %d" filename (fst loc))
    end
  else
    Element(name,attrs,children)}

let text ~loc l = {loc; c = Text(l)}
let comment ~loc c = {loc; c = Comment(c)}
let pi ~loc s1 s2 = {loc; c = PI(s1,s2)}
let xml ~loc x = {loc; c = Xml(x)}

let parse_html ~filename s =
  let report (line,col) err =
    Printf.eprintf "File %S, line %d, characters %d:\n %s\n%!"
      filename line col (Error.to_string err);
    exit 1;
  in
  let element = element ~filename in
  s |> parse_xml ~report ~context:`Document |>
    trees_with_loc ~element ~text ~comment ~pi ~xml ~doctype

exception Incomplete

let trees_to_ocaml ~filename t =
  let buf = Buffer.create 4096 in
  let filter = ref None in
  let in_html = ref false in
  let pr buf fmt = Printf.bprintf buf fmt in
  let of_elems buf top acc =
    let args = ref [] in
    let fn = function
      | `End_element(name) -> print_closing name
      | `Start_element(name,attrs) ->
         let r, nargs = print_tag name attrs in
         args := !args @ nargs; r
      | (`Comment _ | `Doctype _ | `PI _ | `Text _ | `Xml _) as s  ->
         let r = ref (Some s) in
         let fn () = let x = !r in r := None; x in
         stream fn |> write_html |> to_string
    in
    if acc <> [] then
      begin
        let s = String.trim (String.concat "" (List.map fn (List.rev acc))) in
        let args = !args in
        if top then
          begin
            if s <> "" then
              begin
                pr buf "let _ = printf %S " s;
                List.iter (fun a -> pr buf "(%s)" a) args;
                pr buf ";;"
              end
          end
        else
          if args = [] then
            pr buf "%S" s
          else
            begin
              pr buf "(Printf.sprintf %S" s;
              List.iter (fun a -> pr buf "(%s)" a) args;
              pr buf ")"
            end
      end
  in
  let rec fn depth acc stack =
    let top = depth = 0 in
    let newline depth =
      if depth = 0 then pr buf "\n";
      let rec fn d =
        if d > 0 then (pr buf "  "; fn (d - 1))
      in
      fn depth
    in
    let gn s stack = fn depth (s::acc) stack in
    match stack with
    | [] -> if top then acc else (of_elems buf top acc; [])
    | e::stack ->
       match e.c with
       | Include _ -> failwith "include outside caml"
       | Caml(attrs, sons) ->
          let hn buf i x =
            newline (if i = 0 then depth else depth + 1);
            match x.c with
            | Text l -> pr buf "\n#%d %S\n" (fst x.loc) filename;
                        List.iter (pr buf "%s") l
            | Include s ->
               let s = Filename.(concat (dirname filename) s) in
               let ch = open_in s in
               let size = in_channel_length ch in
               let bytes = Bytes.make size ' ' in
               let _ = input ch bytes 0 size in
               pr buf "%S" (Bytes.unsafe_to_string bytes)
            | _      -> of_elems buf false (fn (depth + 1) [] [x]);
          in
          if not !in_html && !filter = None &&
               List.exists (fun ((_,s),v) ->
                   String.lowercase_ascii s = "prelude"
                   && String.lowercase_ascii v = "true") attrs then
            begin
              let buf = Buffer.create 1024 in
              List.iteri (hn buf) sons;
              filter := Some (Buffer.contents buf);
              fn depth acc stack
            end
          else
            begin
              of_elems buf top acc;
              List.iteri (hn buf) sons;
              newline depth;
              fn depth [] stack
            end;
       | Element (n,a,sons) ->
          if snd n = "head" then in_html := true;
          gn (`Start_element(n,a)) (sons @ noloc (End_element n) :: stack)
       | End_element(name) -> gn (`End_element(name)) stack
       | Doctype d -> gn (`Doctype d) stack
       | Text l -> gn (`Text l) stack
       | Comment c -> gn (`Comment c) stack
       | PI (s,u) -> gn (`PI (s,u)) stack
       | Xml x -> gn (`Xml x) stack
  in
  of_elems buf true (fold (fun acc x -> fn 0 acc [x]) [] t);
  (*if not !in_html then raise Incomplete;*)
  (Buffer.contents buf, !filter)

(*
let test =
  {html|<!DOCTYPE html><html><head><title>coucou</title></head>
   <ML>let tmp = "background: black;"</ML>
   <body><ML>let x = <h1 style="?tmp">coucou, c'est l'été<ML>^ test ^</ML></h1></ML><ML>ouput_string x</ML></body>|html}

let t = string test |> parse_html |> trees_to_ocaml
 *)
