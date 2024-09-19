open Pacomb
open Entities

type ocaml_type = Str | Top | Prelude | Global
type value = Empty
           | Unquoted of string
           | Single of string
           | Double of string
           | OCaml of Pos.pos * string
           | OptCaml of Pos.pos * string
type attributes = (string option * Html.attr * value) list

type html =
  | Doctype
  | Element of Html.tag * attributes * html list
  | SElement of Html.tag * attributes
  | Text of string
  | CData of html list (* only allowed in foreign elements *)
  | OCaml of Pos.pos * ocaml_type * string

type stack = (Pos.pos * bool * Html.tag * attributes * html list) list

type closed = (Pos.pos * Html.tag) list

let red fmt =
  "\027[31m" ^^ fmt ^^ "\027[0m%!"

let pr_closed fmt closed =
  let open Format in
  let open Pacomb.Pos in
  List.iter
    (fun (pos, tag) ->
      fprintf fmt "%a: %s\n%!"
        (print_pos ~style:OCaml ()) pos
        (Html.tag_to_string tag)) closed

let badElem elt pos closed =
  let name = match elt with
    | Text _ -> "text"
    | CData _ -> "cdata"
    | OCaml _ -> "ocaml"
    | Element(name,_,_) | SElement(name,_) -> Html.tag_to_string name
    | Doctype -> "doctype"
  in
  let open Pos in
  let open Format in
  let str = get_str_formatter () in
  fprintf str (red "%a: bad element %S in\n%a%!")
    (print_pos ~style:OCaml ()) pos name pr_closed closed;
  let msg = flush_str_formatter () in
  Lex.give_up ~msg ()

let badAttr attr pos tag =
  let open Pos in
  let open Format in
  let str = get_str_formatter () in
  fprintf str (red "%a: bad attribute %S in %S\n%!")
    (print_pos ~style:OCaml ()) pos
    (Html.attr_to_string attr)
    (Html.tag_to_string tag);
  let msg = flush_str_formatter () in
  Lex.give_up ~msg ()

let missClosing closed =
  let open Format in
  let str = get_str_formatter () in
  fprintf str (red "%amiss closing\n%!") pr_closed closed;
  let msg = flush_str_formatter () in
  Lex.give_up ~msg ()

let missOpening tag pos closed =
  let open Pos in
  let open Format in
  let str = get_str_formatter () in
  fprintf str (red "%a: bad closing tag %S, the following tag(s) are not yet closed:\n%a%!")
       (print_pos ~style:OCaml ()) pos
       (Html.tag_to_string tag)
       pr_closed closed;
  let msg = flush_str_formatter () in
  Lex.give_up ~msg ()

let handle_exception f a =
  let open Pacomb.Pos in
  try f a with
  | Parse_error(buf, pos, msgs) ->
     let red fmt = "\027[31m" ^^ fmt ^^ "\027[0m%!" in
     Format.eprintf (red "%a: parse error\n%!")
       (print_buf_pos ~style:OCaml ()) (buf, pos);
     if msgs <> [] then
       begin
         let open Format in
         let prl ch l = List.iter (fprintf ch "%s") l in
         eprintf "%a\n%!" prl msgs
       end;
     exit 1

let may_children tagc infop =
  let open Html in
  let infoc = Hashtbl.find info_tbl tagc in
  let fn = function
    | Tag(s) -> s = tagc
    | Cat(c) -> List.mem c infoc.categories
    | Transparent -> false
    | Data -> false
  in
  List.exists fn infop.children

let accepted_in elt stack =
  let open Html in
  let gn name' =
    match name' with
    | Foreign _ -> true
    | _ -> let rec fn = function
             | [] -> false
             | (_,_,name,_,_) :: stack ->
                try
                  let infop = Hashtbl.find info_tbl name in
                  may_children name' infop ||
                    (List.mem Transparent infop.children &&
                       fn stack)
                with Not_found -> true
           in
           fn stack
  in
  match elt with
  | Element(name',_,_) | SElement(name',_) -> gn name'
  | Text s -> String.trim s = "" || gn Text
  | _ -> true

let allow_ommit name =
  let open Html in
  match name with
  | Head | Body | Html | Li | Dd | Dt | P | Rt | Rp
    | Optgroup | Option | Colgroup | Caption | Thead
    | Tbody | Tfoot | Tr | Td | Th -> true
  | _ -> false

(* Element that enters the "foreign" mode, in which xml namespace,
   arbitrary attribute and tags may be used. *)
let allowed_foreign = ["math"; "svg"]

let is_allowed_foreign elt =
  match elt with
  | Html.Foreign name ->  List.mem name allowed_foreign
  | _ -> false

let check_foreign ~loc elt foreign_ok =
  match elt with
  | Element((Foreign name),_,_) | SElement(Foreign name,_) ->
     if not ( foreign_ok || List.mem name allowed_foreign)
     then badElem elt loc [];
     true
  | CData _ ->
     if not foreign_ok then badElem elt loc [];
     true
  | _ ->
     foreign_ok

let check_attributes ~loc tag (attrs:attributes) foreign_ok =
  let open Html in
  if foreign_ok then () else
    (* once in foreing land, we know nothing about attributes *)
  match tag with
  | Foreign _ -> ()
  | _ ->
     let fn = function
       | (_, ForeignAttr _, _) -> ()
       | (nsname, name, _) ->
          (match Hashtbl.find attr_tbl name with
           | exception Not_found -> badAttr name loc tag
           | Any -> ()
           | Some l ->
              if not (List.mem tag l) then badAttr name loc tag);
          if nsname <> None &&
               nsname <> Some "xml" && name <> Html.Lang (*xml:lang is valid *)
          then badAttr name loc tag
     in
     List.iter fn attrs

let rec push ~loc ?(closed=[]) elt (stack:stack) = match stack with
  | (loc2, frg, name, attrs, elts) :: stack as stack0 ->
     let frg = check_foreign ~loc elt frg in
     if accepted_in elt stack0 then (loc2, frg, name, attrs, (elt::elts)) :: stack
     else if allow_ommit name then
       push ~loc ~closed:((loc2,name)::closed) elt
         (push ~loc:loc2 (Element(name,attrs,List.rev elts)) stack)
     else badElem elt loc ((loc2,name)::closed)
  | [] -> badElem elt loc closed

let get_frg = function
    (_, frg, _, _, _) :: _ -> frg
  | [] -> assert false

let rec pop ~(loc:Pos.pos) ?(closed=[]) name stack = match stack with
  | (_, _, name', attrs, elts) :: stack when name = name' ->
     push ~loc (Element(name,attrs,List.rev elts)) stack
  | (loc2, _, name', attrs, elts) :: stack when allow_ommit name' ->
     pop ~loc ~closed:((loc2,name')::closed) name
       (push ~loc:loc2 (Element(name',attrs,List.rev elts)) stack)
  | (loc, _, name, _, _)::_ ->
     missOpening name loc ((loc,name)::closed)
  | [] -> assert false

let root = Html.Foreign "ROOT"
let rec pop_all ?(closed=[]) stack = match stack with
  | [_, _, r, [], elts] -> assert (r == root); List.rev elts
  | (loc2, _, name', attrs, elts) :: stack when allow_ommit name' ->
     pop_all ~closed:((loc2,name')::closed)
       (push ~loc:loc2 (Element(name',attrs,List.rev elts)) stack)
  | (loc, _, name, _, _)::_ ->
     missClosing ((loc,name)::closed)
  | [] -> assert false

let re_from_string name =
  let buf = Buffer.create 16 in
  Buffer.add_string buf {|\(|};
  for i = 0 to String.length name - 1 do
    let c = name.[i] in
    let lc = Char.lowercase_ascii c and uc = Char.uppercase_ascii c in
    if lc = uc then
      Printf.bprintf buf "[%c]" lc
    else
      Printf.bprintf buf "[%c%c]" lc uc
    done;
  Buffer.add_string buf {|\)|};
  Buffer.contents buf

let re_from_list l =
  String.concat {|\||} (List.map re_from_string l)

let is_void name =
  let open Html in
  try
    (Hashtbl.find info_tbl name).children = []
  with
    Not_found -> false

let may_void name =
  let open Html in
  match name with
  | Foreign _ -> true
  | _ -> is_void name

let void_element =
  let r = ref [] in
  Hashtbl.iter (fun key _ -> if is_void key then r := key :: !r) Html.info_tbl;
  List.rev !r

let void_re_tag = re_from_list (List.map Html.tag_to_string void_element)

let is_raw name =
  let open Html in
  try
    List.mem Data (Hashtbl.find info_tbl name).children
  with
    Not_found -> false

let raw_element =
  let r = ref [] in
  Hashtbl.iter (fun key _ -> if is_raw key then r := key :: !r) Html.info_tbl;
  List.rev !r

let raw_re_tag = re_from_list (List.map Html.tag_to_string raw_element)

let trim s =
  let s0 = String.trim s in
  if s0 = "" then s0 else
    let s0 = if s0.[0] <> s.[0] then " "^s0 else s0 in
    let s0 = if s0.[String.length s0-1] <> s.[String.length s-1]
             then s0^" " else s0 in
    s0

let prefix_pos pos str =
  let info = Pos.pos_info pos in
  let filename = info.file_name in
  let line = info.start_line in
  let col = info.start_col in
  let blanks = String.make col ' ' in
  Format.sprintf "\n#%d %S\n%s%s" line filename blanks str

let double_percent str =
  let buf = Buffer.create (String.length str + 16) in
  String.iter (fun c -> if c = '\001' then Buffer.add_char buf '%'
                        else Buffer.add_char buf c;
                        if c = '%' then Buffer.add_char buf '%') str;
  Buffer.contents buf

let pr_args fmt args =
  List.iter (fun (pos,s) -> Printf.bprintf fmt "(%s)" (prefix_pos pos s)) args

let print_tag buf args ?(self=false) name attributes =
  let attributes =
    attributes
    |> List.map (fun (nmspace, name, (value : value)) ->
           let name = match nmspace with
             | None -> " "^Html.attr_to_string name
             | Some s -> " "^s^":"^Html.attr_to_string name
           in
           match (value : value) with
             | Empty -> name
             | Unquoted s -> name^"="^s
             | Single s -> name^"='"^s^"'"
             | Double s -> name^"=\""^s^"\""
             | OCaml (pos,s) -> args := (pos, s) :: !args; name^"=\"\001s\""
             | OptCaml (pos,s) ->
                let s = Printf.sprintf "Option.value ~default:\"\" %s" s in
                args := (pos, s) :: !args; " \001s")
    |> String.concat ""
  in
  Printf.bprintf buf "<%s%s%s>" (Html.tag_to_string name) attributes
    (if self && not (is_void name) then "/" else "")

let print_closing buf name =
  Printf.bprintf buf "</%s>" (Html.tag_to_string name)

let html_to_string html =
  let buf = Buffer.create 1024 in
  let args = ref [] in
  let rec fn upper = function
    | OCaml(pos,Str,s) -> args := (pos,s) :: !args; Buffer.add_string buf "\001s"
    | OCaml(_,_,_) -> assert false
    | Text s -> Buffer.add_string buf (if upper <> Html.Pre then trim s else s)
    | CData contents ->
       Buffer.add_string buf "<![CDATA[";
       List.iter (fn (Html.Foreign "CDATA")) contents;
       Buffer.add_string buf "]]>";
    | Element(name,attrs,contents) ->
       print_tag buf args name attrs;
       List.iter (fn name) contents;
       if not (allow_ommit name) then print_closing buf name
    | Doctype ->
       Buffer.add_string buf "<!DOCTYPE html>\n" (* newline forced by spec *)
    | SElement(name,attrs) ->
       print_tag buf args ~self:true name attrs;

  in
  List.iter (fn root) html;
  (Buffer.contents buf, List.rev !args)

let output buf cbuf args =
  let ls = List.rev !args in
  args := [];
  let contents = Buffer.contents cbuf in
  Buffer.reset cbuf;
  if contents = "" then ()
  else if ls = [] then
    Printf.bprintf buf "let _ = Out.echo %S;;\n" contents
  else
    Printf.bprintf buf "let _ = Out.printf %S %a;;\n"
      (double_percent contents) pr_args ls


let top_to_string html =
  let buf = Buffer.create 1024 in
  let cbuf = Buffer.create 1024 in
  let args = ref [] in
  let output = output buf cbuf in
  let globals = ref [] in
  let prelude = ref [] in
  let rec fn upper = function
    | OCaml(pos,Top,s) ->
       output args;
       Buffer.add_string buf (prefix_pos pos s);
       Buffer.add_string buf ";;\n"
    | OCaml(pos,Str,s) ->
       args := (pos,s) :: !args;
       Buffer.add_string cbuf "\001s"
    | OCaml(pos,Prelude,s) ->
       prelude := (prefix_pos pos s) :: !prelude
    | OCaml(pos,Global,s) ->
       globals := (prefix_pos pos s) :: !globals
    | Text s -> Buffer.add_string cbuf (if upper <> Html.Pre then trim s else s)
    | CData contents ->
       Buffer.add_string cbuf "<![CDATA[";
       List.iter (fn (Html.Foreign "CDATA")) contents;
       Buffer.add_string cbuf "]]>";
    | Element(name,attrs,contents) ->
       print_tag cbuf args name attrs;
       List.iter (fn name) contents;
       print_closing cbuf name
    | Doctype ->
       Buffer.add_string cbuf "<!DOCTYPE html>\n" (* newline forced by spec *)
    | SElement(name,attrs) ->
       print_tag cbuf args ~self:true name attrs;
  in
  List.iter (fn root) html;
  output args;
  let globals = String.concat "\n" (List.rev !globals) in
  let prelude = String.concat "\n" (List.rev !prelude) in
  (Buffer.contents buf, globals, prelude)

let is_alphanum = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true
  | _ -> false

let contains_ambiguous_ampersand str =
  let p = ref 0 in
  try
    while true do
      let idx = String.index_from str !p '&' in
      p := idx + 1;
      while !p < String.length str && is_alphanum str.[!p] do
        incr p
      done;
      if !p < String.length str && str.[!p] = ';' &&
           not (Hashtbl.mem tbl (String.sub str idx (!p - idx + 1)))
      then raise Exit;
    done;
    assert false
  with
  | Not_found -> false
  | Exit -> true
