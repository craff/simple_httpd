(*
 * The exception [Unclosed_comment(is_string, buf, pos)] is raised when
 * the comment (resp. string litteral in a comment) at position [pos] in
 * [buf] is not closed.
 *)

let _ = if Array.length Sys.argv <> 2 then
          begin
            Printf.eprintf "usage: %s filename\n%!" Sys.argv.(0);
            exit 1;
          end

let filename = Sys.argv.(1)

let is_html = function
  | 'h' :: 't' :: 'm' :: 'l' :: _ -> true
  | _ -> false

let get_fragment = function
  | 'h' :: 't' :: 'm' :: 'l' :: '_' :: l ->
     let buf = Buffer.create 16 in
     List.iter (fun c -> Buffer.add_char buf c) l;
     let frag = Buffer.contents buf in
     Some (`Fragment frag)
  | _ -> None

let ocaml_parse buf out =
  let linenum = ref 1 in
  let linebeg = ref 0 in
  let rec fn state stack curr =
    (*let str = match state with
        `Ini -> "ini"
      | `Chr1 _ -> "chr1"
      | `Str _ -> "str"
      | _      -> "other"
    in
    Printf.eprintf "==> %d %c %s\n%!" curr buf.[curr] str;*)
    let error msg l c =
      Printf.eprintf "File %S, line %d, character %d:\n  %s\nFile %S, line \
                      %d, character %d:\n  this might be the unmatched delimiter%!"
        filename !linenum (curr - !linebeg) msg filename l c;
      exit 1
    in
    let c = buf.[curr] in
    if c = '\n' then (incr linenum; linebeg := curr);
    let pos () = (!linenum, curr - !linebeg - 1, curr) in
    let next = curr + 1 in
    let output (_,_,p) s =
      if s = [] then (
        try output_string out (String.sub buf p (next - p)); flush stdout
        with Invalid_argument _ -> assert false);
      fn `Ini s next
    in
    match (state, (stack : (int * int * int) list), c) with
    (* Comment opening. *)
    | (`Ini      , _   , '('     ) -> fn (`Opn(pos ())) stack next
    | (`Opn(p)   , _   , '*'     ) -> fn `Ini (p::stack) next
    | (`Opn _    , _   , '"'     ) -> if stack = [] then output_char out '(';
                                      fn (`Str(pos())) stack next
    | (`Opn _    , _   , '\''    ) -> if stack = [] then output_char out '(';
                                      fn (`Chr1(pos())) stack next
    | (`Opn _    , _   , '{'     ) -> if stack = [] then output_char out '(';
                                      fn (`SOp([],pos())) stack next
    | (`Opn(_)   , _   , '\n'    ) -> if stack = [] then output_string out "(\n";
                                      fn `Ini stack next
    | (`Opn(_)   , _   , _       ) -> if stack = [] then output_char out '(';
                                      fn `Ini stack curr
    (* String litteral (including the # rules). *)
    | (`Ini      , _   , '"'     ) -> fn (`Str(pos())) stack next
    | (`Str(p)   , _   , '"'     ) -> output p stack
    | (`Str(p)   , _   , '\\'    ) -> fn (`Esc(p)) stack next
    | (`Esc(p)   , _   , _       ) -> fn (`Str(p)) stack next
    | (`Str(l,c,_), _  , '\255'  ) -> error "unclosed string" l c
    | (`Str(_)   , _   , _       ) -> fn state stack next
    (* Char litteral  *)
    | (`Ini      , _   , '\''    ) -> fn (`Chr1(pos ())) stack next
    | (`Chr2(p)   , _   , '\''   ) -> output p stack
    | (`Chr1(p)   , _   , '\\'   ) -> fn (`EsC(p)) stack next
    | (`EsC(p)   , _   , '0'..'9') -> fn (`EsD(p)) stack next
    | (`EsC(p)   , _   , _       ) -> fn (`Chr2(p)) stack next
    | (`EsD(p)   , _   , '0'..'9') -> fn (`EsD(p)) stack next
    | (`EsD(p)   , _   , _       ) -> fn (`Chr2(p)) stack next
    | (`Chr1(l,c,_), _  , '\255' ) -> error "unclosed char" l c
    | (`Chr2(l,c,_), _  , '\255' ) -> error "unclosed char" l c
    | (`Chr1(p)   , _   , _      ) -> fn (`Chr2(p)) stack next
    | (`Chr2(p)   , _   , _      ) -> output p stack
    (* Delimited string litteral in a comment. *)
    | (`Ini      , _   , '{'     ) -> fn (`SOp([],pos ())) stack next
    | (`SOp(l,p) , _   , 'a'..'z')
    | (`SOp(l,p) , _   , '_'     ) -> fn (`SOp(c::l,p)) stack next
    | (`SOp(_,(l,c,_)),_, '\255' ) -> error "unclosed string" l c
    | (`SOp(l,p) , _   , '|'     ) -> fn (`SIn(List.rev l,p)) stack next
    | (`SOp(_,p) , _   , _       ) -> output p stack
    | (`SIn(l,(p:int * int * int)) , _   , '|'     ) -> fn (`SCl(l,(l,p))) stack next
    | (`SIn(_,(l,c,_)),_, '\255' ) -> error "unclosed string" l c
    | (`SIn(_,_) , _   , _       ) -> fn state stack next
    | (`SCl([],(b : char list * (int * int * int))), _   , '}'     ) ->
       let ln, col, pos = snd b in
       let init_pos = (ln - 1, col) in
       if fst b = ['c';'h';'a';'m';'l'] && stack = [] then
         begin
           let start = pos + 7 in
           let end_  = curr  - 6 in
           let str = String.sub buf start (end_ - start) in
           let (ml, global, prelude) =
             Markup.string str
             |> Html52.parse_html ~dynamic:true ~init_pos ~filename
             |> Html52.trees_to_ocaml ~dynamic:true ~init_pos ~filename
           in
           assert (global = "");
           Printf.fprintf out "(fun [@warning \"-27\"] request headers ->
             let module M = struct
               let [@warning \"-32\"] cookies = Cookies.empty
               %s
             end in let open [@warning \"-33\"] M in
             let input =
               Input.of_output (fun [@warning \"-26..27\"] ((module Output) as output) ->
                 let open [@warning \"-33\"] Output in
                 let module M = struct %s end in
                 ()) in
            ( headers, cookies, input ))\n%!"
             prelude ml;
           fn `Ini stack next
         end
       else if is_html (fst b) && stack = [] then
         begin
           let len = List.length (fst b) in
           let start = pos + len + 2 in
           let end_  = curr  - len - 1 in
           let str = String.sub buf start (end_ - start) in
           let context = get_fragment (fst b) in
           let (ml, global, prelude) =
             Markup.string str
             |> Html52.parse_html ?context ~dynamic:true ~init_pos ~filename
             |> Html52.trees_to_ocaml ~dynamic:true ~init_pos ~filename
           in
           assert (global = "");
           assert (prelude = "");
           Printf.fprintf out "(fun [@warning \"-26..27\"] ((module Output : Html.Output) as
  output) ->
                                let open [@warning \"-33\"] Output in
                                let module _ = struct %s end in ())" ml;
           fn `Ini stack next
         end
       else output (snd b) stack
    | (`SCl(_,(_,(l,c, _))),_,'\255') -> error "unclosed string" l c
    | (`SCl([],b), _   , _       ) -> fn (`SIn(b)) stack next
    | (`SCl(c::l,b) , _, c'      ) -> if c = c' then
                                        fn (`SCl(l, b)) stack next
                                      else
                                        fn (`SIn(b)) stack next
    (* Comment closing. *)
    | (`Ini      , _::_, '*'     ) -> fn `Cls stack next
    | (`Cls      , _::_, '*'     ) -> fn `Cls stack next
    | (`Cls      , _::_, '"'     ) -> fn (`Str(pos())) stack next (*#*)
    | (`Cls      , _::_, '\''    ) -> fn (`Chr1(pos())) stack next (*#*)
    | (`Cls      , _::_, '{'     ) -> fn (`SOp([],pos())) stack next (*#*)
    | (`Cls      , p::s, ')'     ) -> output p s
    | (`Cls      , _::_, _       ) -> fn `Ini stack next
    | (`Cls      , []  , _       ) -> assert false (* Impossible. *)
    (* Comment contents (excluding string litterals). *)
    | (`Ini      ,(l,c,_)::_,'\255') -> error "unclosed comment" l c
    | (`Ini      , _   , '\255'  ) -> ()
    | (`Ini      , _::_, _       ) -> fn `Ini stack next
    | (`Ini      , []  , _       ) -> output_char out c;
                                     fn state stack next
  in
  fn `Ini [] 0

let ch = open_in filename
let size = in_channel_length ch
let buf = Bytes.create (size+1)
let _ = assert (input ch buf 0 size = size); Bytes.set buf size '\255'
let _ = Printf.printf "#1 %S\n" filename
let _ = ocaml_parse (Bytes.unsafe_to_string buf) stdout
