open Pacomb
open Chaml

let file_path = ref ""
let do_with_filename filename fn =
  let save = !file_path in
  file_path := Filename.dirname filename;
  try
    let r = fn () in file_path := save; r
  with e -> file_path := save; raise e

let blank = Blank.from_charset Charset.empty

type mode = { cls : string option (* waiting for a closing tag |html} of |chaml} *)
            ; top : bool (* allows for <?ml ... ?> and <?prelude ... ?>*)
            ; str : bool (* allows for <?=  ... ?> *)
            ; glb : bool (* allows for <?global ... ?> *)
            }

let mkMode =
  let tbl = Hashtbl.create 128 in
  (fun ~cls ~top ~str ~glb ->
    let mode = { cls; top; str; glb } in
    try Hashtbl.find tbl mode with
    | Not_found -> Hashtbl.add tbl mode mode; mode)

let is_control code = (0x0000 <= code && code <= 0x001F) ||
                     (0x007F <= code && code <= 0x009F)

let is_space code = (code = 0x0009 || code = 0x000A || code = 0x000C || code = 0x000D
                  || code = 0x0020)

let%parser spaces =
    ()   => ()
  ; RE"[ \n\r\t]+" => ()

let%parser one_spaces =
  RE"[ \n\r\t]+" => ()

(* xml name definition from https://www.w3.org/TR/xml11/#NT-Name html is more
   restrictive for tag name, but we use the list of html tag names for that
   (see https://html.spec.whatwg.org/multipage/syntax.html#syntax-tag-name).

   For attribute name, html is more permissive (see
   https://html.spec.whatwg.org/multipage/syntax.html#attributes-2), but it is
   not possible to use that extra freedom in practice:
   - html attribute names are from a specific list
   - foreign tags (like svg and its decendant) are xml compliant

   NOTE: xml allows for ':' in the name, we treat it as a name space separator.
 *)

(* TODO: implement this as a terminal, not a grammar *)
let is_start_char c =
    let code = Uchar.to_int c in
       (0x41 <= code && code <= 0x5A) (* A-Z *)
    || code = 0x5F (* _ *)
    || (0x61 <= code && code <= 0x7A) (* a-z *)
    || (0xC0 <= code && code <= 0xD6)
    || (0xD8 <= code && code <= 0xF6)
    || (0xF8 <= code && code <= 0x2FF)
    || (0x370 <= code && code <= 0x37D)
    || (0x37F <= code && code <= 0x1FFF)
    || (0x200C <= code && code <= 0x200D)
    || (0x2070 <= code && code <= 0x218F)
    || (0x2C00 <= code && code <= 0x2FEF)
    || (0x3001 <= code && code <= 0xD7FF)
    || (0xFDF0 <= code && code <= 0xFFFD)
    || (0x10000 <= code && code <= 0xEFFFF)

let is_name_char c = is_start_char c ||
    let code = Uchar.to_int c in
        code = 0x2D (* - *)
     || code = 0x2E (* . *)
     || (0x30 <= code && code <= 0x39) (* 0-9 *)
     || code = 0xB7
     || (0x300 <= code && code <= 0x36F)
     || (0x203F <= code && code <= 0x2040)

let start_char = Lex.sub (Lex.any_utf8 ()) is_start_char
let name_char = Lex.sub (Lex.any_utf8 ()) is_name_char

let name =
  Grammar.term (
      Lex.seq start_char (
          Lex.star name_char
            Queue.create
            (fun q c -> Queue.add c q; q))
        (fun c q ->
          let r = Buffer.create 16 in
          Buffer.add_string r (Utf8.encode c);
          Queue.iter (fun c -> Buffer.add_string r (Utf8.encode c)) q;
          Buffer.contents r))

(* from https://html.spec.whatwg.org/multipage/syntax.html#attributes-2 *)
let unquoted_attribute_char = Lex.sub (Lex.any_utf8 ())
  (fun c ->  let code = Uchar.to_int c in
             not (is_space code || code = 0x0022 (*'"'*) || code = 0x0027 (*'*)
             || code = 0x003C (*<*) || code = 0x003D (*=*) || code = 0x003E (*>*)
             || code = 0x0060 (*`*)))

let unquoted_value =
  Grammar.term (
      Lex.seq unquoted_attribute_char (
          Lex.star unquoted_attribute_char
            Queue.create
            (fun q c -> Queue.add c q; q))
        (fun c q ->
          let r = Buffer.create 16 in
          Buffer.add_string r (Utf8.encode c);
          Queue.iter (fun c -> Buffer.add_string r (Utf8.encode c)) q;
          Buffer.contents r))

(* from https://html.spec.whatwg.org/multipage/syntax.html#attributes-2 *)
let  single_attribute_char = Lex.sub (Lex.any_utf8 ())
  (fun c ->  let code = Uchar.to_int c in
             code <> 0x0027)

let single_quoted_value =
  Grammar.term
    (Lex.seq
       (Lex.seq2 (Lex.char '\'')
          (Lex.star single_attribute_char
             Queue.create
             (fun q c -> Queue.add c q; q)))
       (Lex.char '\'')
       (fun q _ ->
         let r = Buffer.create 16 in
         Queue.iter (fun c -> Buffer.add_string r (Utf8.encode c)) q;
         Buffer.contents r))

(* from https://html.spec.whatwg.org/multipage/syntax.html#attributes-2 *)
let  double_attribute_char = Lex.sub (Lex.any_utf8 ())
  (fun c ->  let code = Uchar.to_int c in
             code <> 0x0022)

let double_quoted_value =
  Grammar.term
    (Lex.seq
       (Lex.seq2 (Lex.char '"')
          (Lex.star double_attribute_char
             Queue.create
             (fun q c -> Queue.add c q; q)))
       (Lex.char '"')
       (fun q _ ->
         let r = Buffer.create 16 in
         Queue.iter (fun c -> Buffer.add_string r (Utf8.encode c)) q;
         Buffer.contents r))

let%parser attribute_value =
  (str :: unquoted_value)    =>
    (if contains_ambiguous_ampersand str then
       Lex.give_up ~msg:"ambiguous ampersand in attribute value" ();
     Unquoted str)
  ; (str :: single_quoted_value) =>
    (if contains_ambiguous_ampersand str then
       Lex.give_up ~msg:"ambiguous ampersand in attribute value" ();
     Single str)
  ; (str :: double_quoted_value)  =>
    (if contains_ambiguous_ampersand str then
       Lex.give_up ~msg:"ambiguous ampersand in attribute value" ();
     Double str)

let comment_re =
  {|\([^-<]\|\(-[^-]\)\|\(--[^>!]\)\|\(--![^>]\)\|\(<[^!]\)\|\(<![^-]\)\|\(<!-[^-]\)\)+|}

let%parser comment =
  "<!--" (c:: RE comment_re) "-->" =>
    if String.starts_with ~prefix:">" c || String.starts_with ~prefix:"->" c
       || String.ends_with ~suffix:"<!-" c
    then Lex.give_up ~msg:"Illegal comment" ()

let%parser [@warning -39] rec value mode =
    (v :: attribute_value) => v
  ; (mode.str = true) "<?=" (ocaml::ocaml false) "?>" => (OCaml (ocaml_pos, ocaml) : value)

and attributes mode =
    () => []
  ; (attrs::attributes mode) one_spaces (ns:: ~? ((ns::name) ':' => ns)) (name::name)
      (value :: ~? [Empty] (spaces '=' spaces (v::value mode)=>v))
    => (ns, Html.attr_of_string name, value) :: attrs
  ; (mode.str = true) (attrs::attributes mode) one_spaces "<?=" (ocaml::ocaml false) "?>"
    => (None, Html.attr_of_string "",  (OptCaml (ocaml_pos, ocaml) : value)) :: attrs

and tag_name = (name::name) => Html.tag_of_string name

and non_void_raw_name =
  (name::tag_name) =>
    (if is_void name || is_raw name then Lex.give_up (); name)

and tag mode =
  "<" (name::non_void_raw_name) (attrs::attributes mode) spaces ">" =>
     (name, List.rev attrs)

and closing name =
  let re = re_from_string name in
    "</" (__::RE re) ">" => ()

and any_closing =
    "</" (name::tag_name) ">" => name

and void_name =
  (name::tag_name) =>
    (if not (is_void name) then Lex.give_up (); name)

and may_void_name =
  (name::tag_name) =>
    (if not (may_void name) then Lex.give_up (); name)

and void_tag ml =
    '<' (name::void_name) (attrs::attributes ml) spaces ">" =>
      (name, List.rev attrs)

  ; '<' (name::may_void_name) (attrs::attributes ml) spaces "/>" =>
      (name, List.rev attrs)

and raw_tag mode =
  "<" (name::RE raw_re_tag) (attrs::attributes mode) spaces ">" =>
    (let tag = Html.tag_of_string name in
     (name, (tag, List.rev attrs)))

and raw_re tag =
  let r = ref {|\([^<]\|\(<[^/?]\)|} in
  for i = 0 to String.length tag - 1 do
    r := !r ^ Printf.sprintf {|\|\(</%s[^%c]\)|} (String.sub tag 0 i) tag.[i]
  done;
  !r ^ Printf.sprintf {|\|\(</%s[^ \n\t\r\f/>]\)\)+|} tag

and raw_content mode re =
    () => []
  ; (c::raw_content mode re) (text::RE re) => Text(text)::c
  ; (mode.str = true) (c::raw_content mode re) "<?=" (ocaml:: ocaml false) "?>" => OCaml(ocaml_pos,Str,ocaml)::c

and cdata_re = {|\([^]]\|\(\][^]\)\|\(\]\][^>]\)\)+|}

and cdata mode =
  "<![CDATA[" (c::raw_content mode cdata_re) "]]>" => CData c


and doctype_legacy_string =
    "SYSTEM" spaces ("\"about:legacy-compat\"" => ()
                ; "'about:legacy-compat'" => ()) spaces => ()

and doctype =
  (RE (re_from_string "<!DOCTYPE")) spaces (RE (re_from_string "html"))
     spaces (~? doctype_legacy_string) '>' => Doctype

and inner_html tag = (* TODO: could allow frg parameter *)
  let mode = mkMode ~cls:(Some tag) ~top:false ~str:true ~glb:false in
  (lazy content :: content mode false) (__::STR("|"^tag^"}")) => content

and inner_funml tag = (* TODO: could allow frg parameter *)
  let mode = mkMode ~cls:(Some tag) ~top:true ~str:true ~glb:false in
  (lazy content :: content mode false) (__::STR("|"^tag^"}")) => content

and inner_document tag =
  let mode = mkMode ~cls:(Some tag) ~top:true ~str:true ~glb:false in
  (lazy doc :: document mode) (__::STR("|"^tag^"}")) => doc

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
         let set_pos () =
           let (s,n) = !position in
           let (infos, byte_pos) = Input.spos s n in
           let pos_info = Pos.(pos_info (mk_pos byte_pos byte_pos infos)) in
           let linenum = pos_info.start_line in
           Printf.bprintf out "\n#%d %S\n" linenum pos_info.file_name
         in
         match tag with
         | "html" ->
            output stack 6;
            let (s,n) = !position in
            let (e,s,n) = Grammar.partial_parse_buffer (inner_html tag) blank
                            s ~offset:n
            in
            position := (s,n);
            let (ml, args) = html_to_string e in
            if args = [] then
              Printf.bprintf out "(%S : string)" ml
            else
              Printf.bprintf out "(Printf.sprintf %S %a : string)"
                (double_percent ml) pr_args args;
            set_pos ();
            fn `Ini stack
         | "funml" ->
            output stack 7;
            let (s,n) = !position in
            let (e,s,n) = Grammar.partial_parse_buffer (inner_funml tag) blank
                            s ~offset:n in
            position := (s,n);
            let (ml, _, _) = top_to_string e in
              Printf.bprintf out "(function (module Out : Html.Output) ->
                                  let module _ = struct %s end in ())" ml;
            set_pos ();
            fn `Ini stack
         | "chaml" when top ->
            output stack 7;
            let (s,n) = !position in
            let ((ml,global,prelude),s,n) =
              Grammar.partial_parse_buffer (inner_document tag) blank s
                ~offset:n
            in
            position := (s,n);
            assert (global = "");
            Printf.bprintf out
              "(fun [@warning \"-27\"] request headers ->
                let [@warning \"-26..27\"] cookies = [] in
                let module M = struct
                  %s
                end in
                let open [@warning \"-33-60\"] M in
                let headers = (Headers.Content_Type, \"text/html\")::headers in
                let input =
                  Input.of_output (fun [@warning \"-26..27\"]
                    ((module Out) as output) ->
                      let open [@warning \"-33\"] Out in
                      let module M = struct %s end in
                      ())
                in
                ( headers, cookies, input ))"
              prelude ml;
            set_pos ();
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

and ocaml_elt mode =
    (mode.top = true) "<?ml" (ocaml:: ocaml false) "?>" =>
      OCaml(ocaml_pos,Top,ocaml)

  ; (mode.top = true) "<?prelude" (ocaml:: ocaml false) "?>" =>
      OCaml(ocaml_pos,Prelude,ocaml)

  ; (mode.glb = true) "<?global" (ocaml:: ocaml false) "?>" =>
      OCaml(ocaml_pos,Global,ocaml)

  ; (mode.str = true) "<?=" (ocaml:: ocaml false) "?>" =>
      OCaml(ocaml_pos,Str,ocaml)

and text_elt mode =
    (mode.cls = None) (text::RE{|[^<]+|}) => Text(text)

  ; (mode.cls <> None) (text::RE{|\([^<|]\|\(\(|[a-z]+\)+[^}<]\)\)+|}) =>
      Text(text)

  ; (mode.cls <> None) '|' (text::RE"[a-z]+") "}" =>
      (if mode.cls = Some text then Lex.give_up () else Text("|" ^ text ^ "}"))

and elements mode frg =
    () => [(Pos.no_pos, frg, root, [], [])]

  ; (t::elements mode frg) comment => t

  ; (stack :: elements mode frg) (((name,attrs) = tag):: tag mode) =>
      (if is_void name || is_raw name then Lex.give_up ();
       let frg = get_frg stack in
       check_attributes ~loc:_pos name attrs frg;
       let frg = frg || is_allowed_foreign name in
       (tag_pos,frg,name,attrs,[]) :: stack)

  ; (stack::elements mode frg) (name::any_closing) =>
      (if is_void name || is_raw name then Lex.give_up ();
       pop ~loc:name_pos name stack)

  ; (stack::elements mode frg) (((oname,(name,attrs) = tag)) >: raw_tag mode)
      (content::raw_content mode (raw_re oname)) (closing oname) =>
      (let frg = get_frg stack in
       check_attributes ~loc:_pos name attrs frg;
       push ~loc:tag_pos (Element(name, attrs, List.rev content)) stack)

  ; (stack::elements mode frg) (((name,attrs) = tag)::void_tag mode) =>
      (let frg = get_frg stack in
       check_attributes ~loc:_pos name attrs frg;
       push ~loc:tag_pos (SElement(name,attrs)) stack)

  ; (stack::elements mode frg) (ocaml:: ocaml_elt mode) =>
      push ~loc:ocaml_pos ocaml stack

  ; (stack::elements mode frg) (text:: text_elt mode) =>
      push ~loc:text_pos text stack

  ; (stack::elements mode frg) (cdata:: cdata mode) =>
      (let frg = get_frg stack in
       if not frg then Lex.give_up ~msg:"CDATA onlt allowed in foreign content" ();
       push ~loc:cdata_pos cdata stack)

  ; (mode.str = true) (stack::elements mode frg) (d::include_ mode frg) =>
      List.fold_left (fun stack elt -> push ~loc:Pos.no_pos elt stack) stack d

and content mode frg =
    (stack::elements mode frg) => lazy (pop_all stack)

and document mode =
    spaces (dt::doctype) (e::content mode false) =>
      lazy (let lazy e = e in
            if mode.top then
              Chaml.top_to_string (dt::e)
            else
              let (r, _) = Chaml.html_to_string (dt::e) in
              (r, "", ""))
  ; spaces comment (d::document mode) => d

and include_ mode frg = (* very important to cache here! *)
  "<?include" spaces '"' (name::RE{|[^"]*"|}) spaces "?>" =>
    let name = Scanf.unescaped (String.sub name 0 (String.length name - 1)) in
    let name = Filename.concat !file_path name ^ ".htinc" in
    try
      Lazy.force (do_with_filename name (fun () ->
                      Grammar.parse_file (content mode frg) blank name))
    with e ->
          let msg = Format.sprintf "Can not access file %S (%s)" name
                      (Printexc.to_string e) in
          Lex.give_up ~msg ()

let ocaml_parse ~filename ch =
  do_with_filename filename (fun () ->
      handle_exception (Grammar.parse_channel ~filename (ocaml true) blank) ch)

let document_parse ~filename ch =
  do_with_filename filename (fun () ->
      let mode = mkMode ~cls:None ~top:false ~str:false ~glb:false in
      let (r,_,_) =
        handle_exception (fun () -> Lazy.force (Grammar.parse_channel ~filename
                                (document mode) blank ch)) ()
      in
      r)

let content_parse ~filename ch =
  do_with_filename filename (fun () ->
      let mode = mkMode ~cls:None ~top:false ~str:false ~glb:false in
      let r =
        handle_exception (fun () -> Lazy.force (Grammar.parse_channel ~filename
                                (content mode false) blank ch)) ()
      in
      r)

let chaml_parse ~filename ch =
  do_with_filename filename (fun () ->
      let mode = mkMode ~cls:None ~top:true ~str:true ~glb:true in
      handle_exception (fun () -> Lazy.force (Grammar.parse_channel ~filename (document mode) blank ch)) ())
