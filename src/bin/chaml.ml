(*
 * The exception [Unclosed_comment(is_string, buf, pos)] is raised when
 * the comment (resp. string litteral in a comment) at position [pos] in
 * [buf] is not closed.
 *)

let usage () =
  Printf.eprintf "usage: %s [ocaml|html|chaml] filename\n%!" Sys.argv.(0);
  exit 1

let len = Array.length Sys.argv

let _ = if not (len = 2 || len = 3)  then usage ()

let mode = if len = 2 then "ocaml" else Sys.argv.(1)
let filename = Sys.argv.(len - 1)
let ch = open_in filename
let str = match mode with
  | "ocaml" -> Printf.sprintf "#1 %S\n%s" filename (Parser.(ocaml_parse ~filename ch))
  | "chaml" -> let (r,_,_) = Parser.(chaml_parse ~filename ch) in r
  | "html"  -> Parser.(document_parse ~filename ch)
  | _       -> usage ()

let _ = print_endline str
