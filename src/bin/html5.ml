open Markup

(* from https://www.w3.org/TR/REC-html40/index/attributes.html *)
let uri_type = [ "action"; "background" ; "cite"; "classid"; "codebase"
                 ; "data"; "href"; "longdesc"; "profile"; "src"; "usemap" ]

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
               "%s"
             else if len > 1 && value.[0] = '\\' && value.[1] = '$' then
               String.sub value 1 (len - 1)
             else value
           in
           Printf.sprintf " %s=\"%s\"" (snd name) value)
    |> String.concat ""
  in
  (Printf.sprintf "<%s%s>" (name_to_string name) attributes, List.rev !args)

let print_closing name =
  Printf.sprintf "</%s>" (name_to_string name)

type attrs = (name * string) list

let find_attr name (attrs:attrs) =
  snd (List.find
         (fun ((_,n),_) ->
           String.lowercase_ascii n = String.lowercase_ascii name) attrs)

type ml_kind = Global | Prelude | Normal
type tree_c =
  | Doctype of doctype
  | Element of name * attrs * tree list
  | Caml of ml_kind * attrs * tree list (* TODO: will we use attributes ? *)
  | Include of string
  | Text of string list
  | Comment of string
  | PI of (string * string)
  | Xml of xml_declaration
  | End_element of name

and tree = { loc : location; c : tree_c }

let noloc c = { loc = (-1,-1); c }

let doctype loc d = {loc; c = Doctype(d) }

let lower name = String.lowercase_ascii (snd name)

let is_ml name attrs =
  let is_ml =
    lower name = "ml" ||
      (lower name = "script" &&
         List.exists (fun (name, value) ->
             lower name = "type" && (value="ml" || String.starts_with ~prefix:"ml/" value))
           attrs)
  in
  let ml_kind =
    if List.exists (fun (name, value) ->
           lower name = "type" && value = "ml/prelude")
         attrs
    then Prelude
    else if List.exists (fun (name, value) ->
                lower name = "type" && value = "ml/global")
              attrs
    then Global
    else Normal
  in
  (is_ml, ml_kind)

let element ~filename loc name attrs children =
  let (is_ml, ml_kind) = is_ml name attrs in
  {loc; c =
  if is_ml then
    Caml(ml_kind,attrs,children)
  else if lower name = "include" then
    begin
      match children with
      | [{c = Text [s]; _}] -> Include s
      | _ -> failwith (Printf.sprintf "bad include at %s line %d" filename (fst loc))
    end
  else
    Element(name,attrs,children)}

let text loc l = {loc; c = Text(l)}
let comment loc c = {loc; c = Comment(c)}
let pi loc s1 s2 = {loc; c = PI(s1,s2)}
let xml loc x = {loc; c = Xml(x)}

let parse_html ~dynamic:_ ?context ~filename ?(init_pos=(0,0)) s =
  let detailed_report opens (line,col) err =
    match opens, err with
    | ((_,"ml"), _, _)::_, `Bad_token _ -> ()
    | _, `Bad_content _ -> ()
    | _ ->
       let (l0,c0) = init_pos in
       let (line, col) = if line = 0 then (l0, col + c0)
                         else (line + l0, col)
       in
       Printf.eprintf "File %S, line %d, characters %d:\n %s\n%!"
         filename line col (Error.to_string err);
       exit 1;
  in
  let element = element ~filename in
  s |> parse_html ~detailed_report ?context
    |> trees_with_loc ~element ~text ~comment ~pi ~xml ~doctype

exception Incomplete

let trees_to_ocaml ~dynamic ?(caml=false) ~filename ?(init_pos=(0,0)) t =
  let buf = Buffer.create 4096 in
  let prelude = ref [] in
  let global = ref [] in
  let no_prelude = ref false in
  let pr buf fmt = Printf.bprintf buf fmt in
  let of_elems buf top acc =
    let args = ref [] in
    let fn = function
      | `End_element(name) -> print_closing name
      | `Start_element(name,attrs) ->
         let r, nargs = print_tag name attrs in
         args := !args @ nargs; r
      | `Text s ->
         let s = [String.trim (String.concat " " s)] in
         let r = ref (Some (`Text s)) in
         let fn () = let x = !r in r := None; x in
         stream fn |> normalize_text |> write_html |> to_string
      | (`Comment _ | `Doctype _ | `PI _ | `Xml _) as s  ->
         let r = ref (Some s) in
         let fn () = let x = !r in r := None; x in
         stream fn |> normalize_text |> write_html |> to_string
    in
    if acc <> [] then
      begin
        let s = String.trim (String.concat "" (List.map fn (List.rev acc))) in
        let args = !args in
        if top then
          if dynamic then
            begin
              if s <> "" then
                begin
                  pr buf "let _ = printf %S " s;
                  List.iter (fun a -> pr buf "(%s)" a) args;
                  pr buf ";;"
                end
            end
          else pr buf "%s\n" s
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
       | Caml(ml_kind,_, sons) ->
          let hn buf i x =
            newline (if i = 0 then depth else depth + 1);
            match x.c with
            | Text l -> let ln, _col = init_pos in
                        let blk = String.make (snd x.loc - 1) ' ' in
                        pr buf "\n#%d %S\n%s" (fst x.loc + ln) filename blk;
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
          if not !no_prelude && ml_kind <> Normal then
            begin
              let buf = Buffer.create 1024 in
              List.iteri (hn buf) sons;
              begin
                match ml_kind with
                | Prelude ->
                   prelude := Buffer.contents buf :: !prelude;
                | Global ->
                   global := Buffer.contents buf :: !global;
                | Normal -> assert false
              end;
              fn depth acc stack
            end
          else
            begin
              no_prelude := true;
              of_elems buf top acc;
              List.iteri (hn buf) sons;
              newline depth;
              fn depth [] stack
            end;
       | Element (n,a,sons) ->
          if snd n = "body" then no_prelude := true;
          gn (`Start_element(n,a)) (sons @ noloc (End_element n) :: stack)
       | End_element(name) -> gn (`End_element(name)) stack
       | Doctype d -> gn (`Doctype d) stack
       | Text l -> gn (`Text l) stack
       | Comment c -> gn (`Comment c) stack
       | PI (s,u) -> gn (`PI (s,u)) stack
       | Xml x -> gn (`Xml x) stack
  in
  let depth = if caml then 1 else 0 in
  of_elems buf (not caml) (fold (fun acc x -> fn depth acc [x]) [] t);
  (*if not !in_html then raise Incomplete;*)
  ( Buffer.contents buf
  , String.concat "\n" (List.rev !global)
  , String.concat "\n" (List.rev !prelude))

(*
let test =
  {html|<!DOCTYPE html><html><head><title>coucou</title></head>
   <script type="ml">let tmp = "background: black;"</script>
   <body><script type="ml">let x = <h1 style="?tmp">coucou, c'est l'été<script type="ml">^ test ^</script></h1></script><script type="ml">ouput_string x</script></body>|html}

let t = string test |> parse_html |> trees_to_ocaml
 *)
