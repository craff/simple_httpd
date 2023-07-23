(*
  TODO: CDATA
  TODO: template
 *)

open Pacomb
open Chaml

let blank = Blank.from_charset Charset.empty

type mode = { cls : string option (* waiting for a closing tag |html} of |chaml} *)
            ; top : bool (* allows for <?ml ... ?> and <?prelude ... ?>*)
            ; str : bool (* allows for <?=  ... ?> *)
            ; glb : bool (* allows for <?global ... ?> *)
            }

let is_control code = (0x0000 <= code && code <= 0x001F) ||
                     (0x007F <= code && code <= 0x009F)

let is_space code = (code = 0x0009 || code = 0x000A || code = 0x000C || code = 0x000D
                  || code = 0x0020)

let%parser space =
    ' '  => ()
  ; '\t' => ()
  ; '\n' => ()
  ; '\r' => ()
  ; '\t' => ()

let%parser attribute_name_char =
  (c::UTF8) =>
    let code = Uchar.to_int c in
    if is_control code || code = 0x0020 || code = 0x0022
       || code = 0x0027 || code = 0x002F || code =0x003E || code = 0x003D then
      Lex.give_up ();
    Utf8.encode c

let%parser attribute_name =
  (cs :: ~+ attribute_name_char) =>
    String.concat "" cs

let%parser unquoted_attribute_char =
  (c::UTF8) => let code = Uchar.to_int c in
               if is_space code || code = 0x0022 || code = 0x0027 || code = 0x003C
                  || code = 0x003E || code = 0x0060 then Lex.give_up ();
               Utf8.encode c

let%parser single_attribute_char =
  (c::UTF8) => let code = Uchar.to_int c in
               if code = 0x0027 then Lex.give_up ();
               Utf8.encode c

let%parser double_attribute_char =
  (c::UTF8) => let code = Uchar.to_int c in
               if code = 0x0022 then Lex.give_up ();
               Utf8.encode c

let%parser attribute_value =
  (cs :: ~+ unquoted_attribute_char)    =>
    (let str = String.concat "" cs in
     if contains_ambiguous_ampersand str then
       Lex.give_up ~msg:"ambiguous ampersand in attribute value" ();
     Unquoted str)
  ; '\'' (cs :: ~+ single_attribute_char) '\'' =>
    (let str = String.concat "" cs in
     if contains_ambiguous_ampersand str then
       Lex.give_up ~msg:"ambiguous ampersand in attribute value" ();
     Single str)
  ; '"'  (cs :: ~+ double_attribute_char) '"'  =>
    (let str = String.concat "" cs in
     if contains_ambiguous_ampersand str then
       Lex.give_up ~msg:"ambiguous ampersand in attribute value" ();
     Double str)

let%parser tag_name = (n :: RE("[a-zA-Z0-9]+")) => String.lowercase_ascii n

let comment_re =
  {|\([^-<]\|\(-[^-]\)\|\(--[^>!]\)\|\(--![^>]\)\|\(<![^-]\)\|\(<!-[^-]\)\)+|}

let%parser rec comment =
  "<!--" (c:: RE comment_re) "-->" =>
    if String.starts_with ~prefix:">" c || String.starts_with ~prefix:"->" c
       || String.ends_with ~suffix:"<!-" c
    then Lex.give_up ~msg:"Illegal comment" ()

let%parser [@warning -39] rec value mode =
    (v :: attribute_value) => v
  ; (mode.str = true) "<?=" (ocaml::ocaml false) "?>" => (OCaml (ocaml_pos, ocaml) : value)


and attributes mode =
    (~* space) => []
  ; (~+ space) (name::attribute_name) (~* space)
      (value :: ~? [Empty] ('=' (~* space) (v::value mode)=>v))
      (attrs::attributes mode)
    => (name, value) :: attrs

and tag mode =
  "<" (name::tag_name) (attrs::attributes mode) ">" =>
    (if List.mem name void_element then Lex.give_up ();
     if List.mem name raw_element then Lex.give_up ();  (name, attrs))

and raw_tag tag mode =
  "<" (STR tag) (attrs::attributes mode) ">" =>  (tag, attrs)

and raw_re tag =
  let r = ref {|\([^<]\|\(<[^/?]\)|} in
  for i = 0 to String.length tag - 1 do
    r := !r ^ Printf.sprintf {|\|\(</%s[^%c]\)|} (String.sub tag 0 i) tag.[i]
  done;
  !r ^ Printf.sprintf {|\|\(</%s[^ \n\t\r\f/>]\)\)+|} tag

and raw_content mode tag =
    () => ""
  ; (text::RE (raw_re tag)) (c::raw_content) => Text(text)::c
  ; (mode.str = true) "<?=" (ocaml:: ocaml false) "?>" => OCaml(ocaml_pos,Str,ocaml)

and doctype_legacy_string =
  "SYSTEM" (~* space) ("\"about:legacy-compat\"" => ()
                     ; "'about:legacy-compat'" => ()) (~* space) => ()
and doctype =
  "<!DOCTYPE" (~* space) (RE{|[hH][tT][mM][lL]|})
     (~* space) (~? doctype_legacy_string) '>' => Doctype

and closing =
  "</" (name::tag_name) ">" => name

and void_tag ml =
  '<' (name::tag_name) (attrs::attributes ml) (sl :: ~? '/') '>' =>
    (if sl == None && not (List.mem name void_element) then Lex.give_up ();
     (name, attrs))


and inner_html tag =
  let mode = { cls = Some tag; top = false; str = true; glb = false } in
  (content :: content mode) (__::STR("|"^tag^"}")) => content

and inner_funml tag =
  let mode = { cls = Some tag; top = true; str = true; glb = false } in
  (content :: content mode) (__::STR("|"^tag^"}")) => content

and inner_document tag =
  let mode = { cls = Some tag; top = true; str = true; glb = false } in
  (doc :: document mode) (__::STR("|"^tag^"}")) => doc

and ocaml_lexer top s n =
  (* empty terminals are not allowed by pacomb *)
  let (c, _, _) = Input.read s n in
  if c = '\255' then raise Lex.NoParse;
  (* Printf.eprintf "parsing ocaml at %d,%d\n%!" !linenum !colnum; *)
  let out = Buffer.create 1024 in
  let cur = Buffer.create 1024 in
  let output s nb =
    if s = [] then (
      let str = Buffer.sub cur 0 (Buffer.length cur - nb) in
      Buffer.add_string out str;
      Buffer.reset cur;
    );
  in
  let position = ref (s, n) in
  let prev = [|(s,n);(s,n)|] in
  let next () =
    let (s, n) as p = !position in
    prev.(1) <- prev.(0);
    prev.(0) <- p;
    let (c, s, n) = Input.read s n in
    Buffer.add_char cur c;
    position := (s,n);
    c
  in
  let pos () = let (s, n) = !position in Input.spos s n in
  let rec fn ?(c=next ()) state stack =
    let error _msg _l _c =
      (*        Printf.eprintf "File %S, line %d, character %d:\n  %s\nFile %S, line \
                %d, character %d:\n  this might be the unmatched delimiter%!"
                filename !linenum !colnum msg filename l c;*)
      exit 1
    in

    match (state, (stack : Input.spos list), c) with
    (* Comment opening. *)
    | (`Ini      , _   , '('     ) -> fn (`Opn(pos ())) stack
    | (`Opn(p)   , _   , '*'     ) -> fn `Ini (p::stack)
    | (`Opn(_)   , _   , _       ) -> fn `Ini stack ~c
    (* String litteral (including the # rules). *)
    | (`Ini      , _   , '"'     ) -> fn (`Str(pos())) stack
    | (`Str(_)   , _   , '"'     ) -> fn `Ini stack
    | (`Str(p)   , _   , '\\'    ) -> fn (`Esc(p)) stack
    | (`Esc(p)   , _   , _       ) -> fn (`Str(p)) stack
    | (`Str(l,c) , _   , '\255'  ) -> error "unclosed string" l c
    | (`Str(_)   , _   , _       ) -> fn state stack
    (* Char litteral  *)
    | (`Ini      , _   , '\''    ) -> fn (`Chr1(pos ())) stack
    | (`Chr2(_)   , _   , '\''   ) -> fn `Ini stack
    | (`Chr1(p)   , _   , '\\'   ) -> fn (`EsC(p)) stack
    | (`EsC(p)   , _   , '0'..'9') -> fn (`EsD(p)) stack
    | (`EsC(p)   , _   , _       ) -> fn (`Chr2(p)) stack
    | (`EsD(p)   , _   , '0'..'9') -> fn (`EsD(p)) stack
    | (`EsD(p)   , _   , _       ) -> fn (`Chr2(p)) stack
    | (`Chr1(_)  , _   , '\255'  ) -> fn `Ini stack
    | (`Chr2(l,c), _   , '\255'  ) -> error "unclosed char" l c
    | (`Chr1(p)   , _   , _      ) -> fn (`Chr2(p)) stack
    | (`Chr2(_)   , _   , _      ) -> fn `Ini stack
    (* Delimited string litteral in a comment. *)
    | (`Ini      , _   , '{'     ) -> fn (`SOp([],pos ())) stack
    | (`SOp(l,p) , _   , 'a'..'z') -> fn (`SOp(c::l,p)) stack
    | (`SOp(_,(l,c)),_ , '\255'  ) -> error "unclosed string" l c
    | (`SOp(l,p) , _   , '|'     ) ->
       let tag = String.concat "" (List.rev_map (fun c -> String.make 1 c) l) in
       begin
         match tag with
         | "html" ->
            output stack 6;
            let (s,n) = !position in
            let (e,s,n) = Grammar.partial_parse_buffer (inner_html tag) blank s n in
            position := (s,n);
            let (ml, args) = html_to_string e in
            if args = [] then
              Printf.bprintf out "(%S : string)" ml
            else
              Printf.bprintf out "(Printf.sprintf %S %a : string)"
                (double_percent ml) pr_args args;
            fn `Ini stack
         | "funml" ->
            output stack 7;
            let (s,n) = !position in
            let (e,s,n) = Grammar.partial_parse_buffer (inner_funml tag) blank s n in
            position := (s,n);
            let (ml, _, _) = top_to_string e in
              Printf.bprintf out "(function (module Out : Html.Output) ->
                                  let module _ = struct %s end in ())" ml;
            fn `Ini stack
         | "chaml" when top ->
            output stack 7;
            let (s,n) = !position in
            let ((ml,global,prelude),s,n) =
              Grammar.partial_parse_buffer (inner_document tag) blank s n
            in
            position := (s,n);
            assert (global = "");
            Printf.bprintf out
              "(fun [@warning \"-27\"] request headers ->
                let module M = struct
                  let [@warning \"-32\"] cookies = Cookies.empty
                  %s
                end in
                let open [@warning \"-33\"] M in
                let input =
                  Input.of_output (fun [@warning \"-26..27\"]
                    ((module Out) as output) ->
                      let open [@warning \"-33\"] Out in
                      let module M = struct %s end in
                      ())
                in
                ( headers, cookies, input ))\n%!"
              prelude ml;
            fn `Ini stack
       | _ ->
          fn (`SIn(List.rev l,p)) stack
       end
    | (`SOp(_,_) , _   , _       ) -> fn `Ini stack ~c
    | (`SIn(l,p) , _   , '|'     ) -> fn (`SCl(l,(l,p))) stack
    | (`SIn(_,(l,c)),_ , '\255'  ) -> error "unclosed string" l c
    | (`SIn(_,_) , _   , _       ) -> fn state stack
    | (`SCl([],_), _   , '}'     ) -> fn `Ini stack
    | (`SCl(_,(_,(l,c))),_,'\255') -> error "unclosed string" l c
    | (`SCl([],b), _   , _       ) -> fn (`SIn(b)) stack
    | (`SCl(c::l,b) , _, c'      ) -> if c = c' then
                                        fn (`SCl(l, b)) stack
                                      else
                                        fn (`SIn(b)) stack
    (* Comment closing. *)
    | (`Ini      , _::_, '*'     ) -> fn `Cls stack
    | (`Cls      , _::s, ')'     ) -> fn `Ini s
    | (`Cls      , _::_, _       ) -> fn `Ini stack ~c
    | (`Cls      , []  , _       ) -> assert false
    (* Comment contents (excluding string litterals). *)
    | (`Ini      ,(l,c)::_,'\255') -> error "unclosed comment" l c
    | (`Ini      , []   , '\255'  ) -> (1,prev.(0))
    (* End of ocaml *)
    | (`Ini      , []  , '?'     ) -> fn `End stack
    | (`End      , []  , '>'     ) -> (2,prev.(1))
    | (`End      , []  , _       ) -> fn `Ini stack ~c
    | (`End      , _::_, _       ) -> assert false
    (* Other char *)
    | (`Ini      , _  , _        ) -> fn `Ini stack
  in
  let (r,(s,n)) = fn `Ini [] in
  output [] r;
  (Buffer.contents out,s,n)

and ocaml top = (* ocaml definition *)
  () => ""
  ; (ocaml :: Grammar.term (Lex.{ n = "OCAML"
                                ; c = Charset.full
                                ; a = custom (ocaml_lexer top)
                                ; f = (ocaml_lexer top) })) => ocaml

and element mode : html Grammar.t =
  ((name,attrs) :: tag mode) (content::content mode) (name'::closing) =>
    (if name <> name' then Lex.give_up ~msg:"mismatched tags" ();
     Element(name, attrs, content))
  ; ((name,attrs) :: raw_tag "script" mode) (content::raw_content "script") (name'::closing) =>
    (if name <> name' then Lex.give_up ~msg:"mismatched tags" ();
     Element(name, attrs, [Text content]))
  ; ((name,attrs) :: raw_tag "style" mode) (content::raw_content "style") (name'::closing) =>
    (if name <> name' then Lex.give_up ~msg:"mismatched tags" ();
     Element(name, attrs, [Text content]))
  ; ((name,attrs) :: raw_tag "textarea" mode) (content::raw_content "textarea") (name'::closing) =>
    (if name <> name' then Lex.give_up ~msg:"mismatched tags" ();
     Element(name, attrs, [Text content]))
  ; ((name,attrs) :: raw_tag "title" mode) (content::raw_content "title") (name'::closing) =>
    (if name <> name' then Lex.give_up ~msg:"mismatched tags" ();
     Element(name, attrs, [Text content]))
  ; ((n,a)::void_tag mode) => SElement(n,a)
  ; (mode.top = true) "<?ml" (ocaml:: ocaml false) "?>" => OCaml(ocaml_pos,Top,ocaml)
  ; (mode.top = true) "<?prelude" (ocaml:: ocaml false) "?>" => OCaml(ocaml_pos,Prelude,ocaml)
  ; (mode.glb = true) "<?global" (ocaml:: ocaml false) "?>" => OCaml(ocaml_pos,Global,ocaml)
  ; (mode.str = true) "<?=" (ocaml:: ocaml false) "?>" => OCaml(ocaml_pos,Str,ocaml)
  ; (mode.cls = None) (text::RE{|[^<]+|}) => Text(text)
  ; (mode.cls <> None) (text::RE{|\([^<|]\|\(\(|[a-z]+\)+[^}<]\)\)+|}) => Text(text)
  ; (mode.cls <> None) "|" (text::RE"[a-z]+") "}" =>
      (if mode.cls = Some text then Lex.give_up () else Text("|" ^ text ^ "}"))

and content mode =
    () => []
  ; (e::element mode) (c::content mode) => e::c

and document mode = (~* space) (dt::doctype) (e::content mode) =>
                    if mode.top then
                      Chaml.top_to_string (dt::e)
                    else
                      let (r, _) = Chaml.html_to_string (dt::e) in
                      (r, "", "")

let ocaml_parse ~filename ch =
  Pos.handle_exception (Grammar.parse_channel ~filename (ocaml true) blank) ch

let document_parse ~filename ch =
  let mode = { cls = None; top = false; str = false; glb = false } in
  let (r,_,_) =
    Pos.handle_exception (Grammar.parse_channel ~filename
                            (document mode) blank) ch
  in
  r

let chaml_parse ~filename ch =
  let mode = { cls = None; top = true; str = true; glb = true } in
  Pos.handle_exception (Grammar.parse_channel ~filename (document mode) blank) ch
