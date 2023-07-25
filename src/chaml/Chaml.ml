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

type stack = (string * attributes * html list) list

let metadata = ["base"; "link"; "meta"; "noscript"; "script"; "style"; "template";
                "title" ]

let flow = ["a"; "abbr"; "address"; "area" (*if it is a descendant of a map element*)
           ; "article"; "aside"; "audio"; "b"; "bdi"; "bdo"; "blockquote"; "br"
           ; "button"; "canvas"; "cite"; "code"; "data"; "datalist"; "del"; "details"
           ; "dfn"; "dialog"; "div"; "dl"; "em"; "embed"; "field"; "set"; "figure"
           ; "footer"; "form"; "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "header"; "hgroup"
           ; "hr"; "i"; "iframe"; "img"; "input"; "ins"; "kbd"; "label"
           ; "link" (*if it is allowed in the body*)
           ; "main" (*if it is a hierarchically correct main element*)
           ; "map"; "mark"; "mathml"; "math"; "menu"
           ; "meta" (*if the itemprop attribute is present*)
           ; "meter"; "nav"; "noscript"; "object"; "ol"; "output"; "p"; "picture"
           ; "pre"; "progress"; "q"; "ruby"; "s"; "samp"; "script"; "search"
           ; "section"; "select"; "slot"; "small"; "span"; "strong"; "sub"; "sup"
           ; "svg"; "svgtable"; "table"; "template"; "textarea"; "time"; "u"; "ul"; "var"
           ; "video"; "wbr" ]

let sectioning = ["article"; "aside"; "nav"; "section"]

let heading = ["h1"; "h2"; "h3"; "h4"; "h5"; "h6"
              ; "hgroup" (*if it has a descendant h1 to h6 element*) ]

let phrasing = ["a"; "abbr"; "area" (*if it is a descendant of a map element*)
               ; "audio"; "b"; "bdi"; "bdo"; "br"; "button"; "canvas"; "cite"; "code"
               ; "data"; "datalist"; "del"; "dfn"; "em"; "embed"; "i"; "iframe"
               ; "img"; "input"; "ins"; "kbd"; "label"
               ; "link" (*if it is allowed in the body*)
               ; "map"; "mark"; "mathml"; "math"
               ; "meta" (*if the itemprop attribute is present*)
               ; "meter"; "noscript"; "object"; "output"; "picture"; "progress"
               ; "qruby"; "s"; "samp" ; "script"; "select"; "slot"; "small"; "span"
               ; "strong"; "sub"; "sup"; "svg"; "template"; "textarea"; "time"; "uvar"
               ; "video"; "wbr" ]

let address =
  List.filter
    (fun x -> not (List.mem x (sectioning @ heading @ ["header"; "footer"])))
    flow

let rec accepted_in elt name =
  match elt with
  | Element(name',_,_) | SElement(name',_) ->
     List.mem name ["script"; "template"] ||
     begin
       let chk = List.mem name' in
       match name with
       | "" -> true
       | "html" -> chk ["head"; "body"]
       | "head" -> chk metadata
       | "a" | "em" | "strong" | "small" | "s" | "cite" | "q" | "dfn" | "abbr"
         | "rt" | "data" | "time" | "code" | "var" | "samp" | "kbd" | "sub" | "sup"
         | "i" | "b" | "u" | "mark" | "bdi" | "bdo" | "span" | "br" | "wbr"
         | "ins" | "del" | "object" | "map" | "label" | "button" | "output"
         | "progress" | "meter" | "legend" | "summary" | "noscript" | "canvas"
         -> chk phrasing
       (* TODO: time attribute ?*)
       (* TODO: check object *)
       (* TODO: button: no interactive inside *)
       | "body" | "td" | "th" | "article" | "section" | "nav"
         | "aside" | "header" | "footer" | "blockquote" | "li" | "dd" | "dialog"
         | "figcaption" | "main" | "search" | "div" | "form" | "caption"
         -> chk flow (* TODO: form in form *)
       | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "p" | "pre"
         -> chk phrasing
       | "hgroup" -> chk ["h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "p" ]
       | "address" -> chk address
       | "ul" | "ol" | "menu" -> chk ["li"]
       | "dl" -> chk ["dt";"dd";"div"] (* TODO: div should be restricted *)
       | "figure" -> chk ("figcaption" :: flow)
       | "ruby" -> chk ("rt" :: "rp" :: phrasing) (* TODO *)
       | "picture" -> chk ["source"; "img"]
       | "video" | "audio"  -> chk ("source"::"track":: phrasing) (* CHECK *)
       | "table" -> chk ["tbody"; "tr"; "thead"; "tfoot"; "caption"; "colgroup" ]
       | "tbody" | "thead" | "tfoot" -> chk ["tr"]
       | "colgroup" -> chk ["col"]
       | "tr" | "rp" -> chk ["td"; "th" ]
       | "select" -> chk ["option"; "optgroup"; "hr"]
       | "optgroup" -> chk ["option"]
       | "fieldset" -> chk ("legend" :: flow)
       | "details" -> chk ("summary" :: flow)
       | "col" | "hr" | "source" | "img" | "iframe" | "embed" | "track" | "area"
         | "input" | "option" | "textarea" -> false
       | _ -> true
     end
  | Text s -> String.trim s = "" || accept_text name
  | _ -> true

and accept_text name = accepted_in (Element("span",[],[])) name

let allow_ommit name = match name with
  | "head" | "body" | "html" | "li" | "dd" | "dt" | "p" | "rt" | "rp"
    | "optgroup" | "option" | "colgroup" | "caption" | "thead"
    | "tbody" | "tfoot" | "tr" | "td" | "th" -> true
  | _ -> false

let badelem elt =
  let name = match elt with
    | Text _ -> "text content"
    | OCaml _ -> "ocaml"
    | Element(name,_,_) | SElement(name,_) -> name
    | Doctype -> "doctype"
  in
  let msg = Printf.sprintf "Bad element %S" name in
  Lex.give_up ~msg ()

let mismatch name =
  let msg = Printf.sprintf "Tag mismatch for %S" name in
  Lex.give_up ~msg ()

let rec push elt (stack:stack) = match stack with
  | (name, attrs, elts) :: stack as stack0 ->
     if accepted_in elt name then
       match elt with
       | Text s when not (accept_text name) ->
          assert (String.trim s = ""); stack0
       | _ -> (name, attrs, (elt::elts)) :: stack
     else if allow_ommit name then push elt (push (Element(name,attrs,List.rev elts)) stack)
     else badelem elt
  | [] -> badelem elt

let rec pop name stack = match stack with
  | (name', attrs, elts) :: stack when name = name' ->
     push (Element(name,attrs,List.rev elts)) stack
  | (name', attrs, elts) :: stack when allow_ommit name' ->
     pop name (push (Element(name',attrs,List.rev elts)) stack)
  | _ -> mismatch name

let rec pop_all stack = match stack with
  | ["", [], elts] -> List.rev elts
  | (name', attrs, elts) :: stack when allow_ommit name' ->
     pop_all (push (Element(name',attrs,List.rev elts)) stack)
  | _ -> mismatch "?"

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

let void_element =
  [ "area"; "base"; "br"; "col"; "embed"; "hr"; "img"
  ; "input" ; "link" ; "meta" ; "source" ; "track" ; "wbr" ]

let is_void name = List.mem (String.lowercase_ascii name) void_element

let void_re_tag = re_from_list void_element

let raw_element = [ "script"; "style"; "textarea"; "title" ]

let is_raw name = List.mem (String.lowercase_ascii name) raw_element

let raw_re_tag = re_from_list raw_element

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
       if not (allow_ommit name) then print_closing buf name
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
