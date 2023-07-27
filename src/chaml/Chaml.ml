open Pacomb
open Entities

type ocaml_type = Str | Top | Prelude | Global
type value = Empty
           | Unquoted of string
           | Single of string
           | Double of string
           | OCaml of Pos.pos * string
type attributes = (Html.attr * value) list

type html =
  | Doctype
  | Element of Html.tag * attributes * html list
  | SElement of Html.tag * attributes
  | Text of string
  | OCaml of Pos.pos * ocaml_type * string

type stack = (Html.tag * attributes * html list) list

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
             | (name,_,_) :: stack ->
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

let badelem elt =
  let name = match elt with
    | Text _ -> "text content"
    | OCaml _ -> "ocaml"
    | Element(name,_,_) | SElement(name,_) -> Html.tag_to_string name
    | Doctype -> "doctype"
  in
  let msg = Printf.sprintf "Bad element %S" name in
  Lex.give_up ~msg ()

let mismatch name =
  let msg = Printf.sprintf "Tag mismatch for %S" (Html.tag_to_string name) in
  Lex.give_up ~msg ()

let rec push elt (stack:stack) = match stack with
  | (name, attrs, elts) :: stack as stack0 ->
     if accepted_in elt stack0 then (name, attrs, (elt::elts)) :: stack
     else if allow_ommit name then push elt (push (Element(name,attrs,List.rev elts)) stack)
     else badelem elt
  | [] -> badelem elt

let rec pop name stack = match stack with
  | (name', attrs, elts) :: stack when name = name' ->
     push (Element(name,attrs,List.rev elts)) stack
  | (name', attrs, elts) :: stack when allow_ommit name' ->
     pop name (push (Element(name',attrs,List.rev elts)) stack)
  | _ -> mismatch name

let root = Html.Foreign "ROOT"
let rec pop_all stack = match stack with
  | [r, [], elts] -> assert (r == root); List.rev elts
  | (name', attrs, elts) :: stack when allow_ommit name' ->
     pop_all (push (Element(name',attrs,List.rev elts)) stack)
  | _ -> mismatch root

let re_from_string name =
  let buf = Buffer.create 16 in
  Buffer.add_string buf {|\(|};
  for i = 0 to String.length name - 1 do
    let c = name.[i] in
    let lc = Char.lowercase_ascii c and uc = Char.uppercase_ascii c in
    if lc = uc then Buffer.add_char buf c else
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
  Printf.sprintf "\n#%d %S\n%s%s" line filename blanks str

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
    |> List.map (fun (name, (value : value)) ->
           let value = match (value : value) with
             | Empty -> ""
             | Unquoted s -> "="^s
             | Single s -> "='"^s^"'"
             | Double s -> "=\""^s^"\""
             | OCaml (pos,s) -> args := (pos, s) :: !args; "=\"\001s\""
           in
           Printf.sprintf " %s%s" (Html.attr_to_string name) value)
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
    | Element(name,attrs,contents) ->
       print_tag buf args name attrs;
       List.iter (fn name) contents;
       if not (allow_ommit name) then print_closing buf name
    | Doctype ->
       Buffer.add_string buf "<!DOCTYPE html>"
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
    | Element(name,attrs,contents) ->
       print_tag cbuf args name attrs;
       List.iter (fn name) contents;
       print_closing cbuf name
    | Doctype ->
       Buffer.add_string cbuf "<!DOCTYPE html>"
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
