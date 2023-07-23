open Pacomb
open Entities

type ocaml_type = Str | Top | Prelude | Global
type name = string
type value = Empty
           | Unquoted of string
           | Single of string
           | Double of string
           | OCaml of Pos.pos * string
type attributes = (name * value) list

type html =
  | Doctype
  | Element of name * attributes * html list
  | SElement of name * attributes
  | Text of string
  | OCaml of Pos.pos * ocaml_type * string

let void_element =
  [ "area"; "base"; "br"; "col"; "embed"; "hr"; "img"
  ; "input" ; "link" ; "meta" ; "source" ; "track" ; "wbr" ]

let raw_element = [ "script"; "style"; "texarea"; "title" ]

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
           Printf.sprintf " %s%s" name value)
    |> String.concat ""
  in
  Printf.bprintf buf "<%s%s%s>" name attributes
    (if self && not (List.mem name void_element) then "/" else "")

let print_closing buf name =
  Printf.bprintf buf "</%s>" name

let html_to_string html =
  let buf = Buffer.create 1024 in
  let args = ref [] in
  let rec fn upper = function
    | OCaml(pos,Str,s) -> args := (pos,s) :: !args; Buffer.add_string buf "\001s"
    | OCaml(_,_,_) -> assert false
    | Text s -> Buffer.add_string buf (if upper <> "pre" then trim s else s)
    | Element(name,attrs,contents) ->
       print_tag buf args name attrs;
       List.iter (fn name) contents;
       print_closing buf name
    | Doctype ->
       Buffer.add_string buf "<!DOCTYPE html>"
    | SElement(name,attrs) ->
       print_tag buf args ~self:true name attrs;

  in
  List.iter (fn "") html;
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
    | Text s -> Buffer.add_string cbuf (if upper <> "pre" then trim s else s)
    | Element(name,attrs,contents) ->
       print_tag cbuf args name attrs;
       List.iter (fn name) contents;
       print_closing cbuf name
    | Doctype ->
       Buffer.add_string cbuf "<!DOCTYPE html>"
    | SElement(name,attrs) ->
       print_tag cbuf args ~self:true name attrs;
  in
  List.iter (fn "") html;
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
