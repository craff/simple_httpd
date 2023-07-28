type mode = OCaml | Inner | Html | Auto | Chaml

let mode = ref OCaml
let set_mode m = Arg.Unit(fun () -> mode:=m)

let spec = [
    ("--ocaml", set_mode OCaml,
     "ocaml preprocessor for ocaml files with embedded html");
    ("--chaml", set_mode Chaml, "ocaml preprocessor chaml files");
    ("--html", set_mode Html, "parse html document");
    ("--inner", set_mode Inner, "parse html fragment");
    ("--auto", set_mode Auto,
     "parse html document if there is a line starting with \"<!DOCTYPE\",\
      or \"<!doctype\" (may read the file twice)")]

let filename = ref None
let set_filename fn =
  match !filename with
  | None -> filename := Some fn
  | Some _ ->
     failwith (Printf.sprintf "%s: only one input file accepted" Sys.argv.(0))

let rec fn ch filename = function
  | OCaml -> Printf.sprintf "#1 %S\n%s" filename (Parser.(ocaml_parse ~filename ch))
  | Chaml -> let (r,_,_) = Parser.(chaml_parse ~filename ch) in r
  | Html  -> Parser.(document_parse ~filename ch)
  | Inner -> fst (Chaml.html_to_string Parser.(content_parse ~filename ch))
  | Auto  ->
     let mode =
       try
         while true do
           let line = input_line ch in
           if String.starts_with ~prefix:"<!DOCTYPE" line
              || String.starts_with ~prefix:"<!doctype" line
           then raise Exit
         done;
         seek_in ch 0;
         Inner
       with Exit -> seek_in ch 0; Html
     in
     fn ch filename mode

let _ =
  try
    Arg.parse spec set_filename
      "usage: %s [ocaml|html|chaml|inner|auto] input-file\n%!";
    let ch, filename =
      match !filename with
      | None -> failwith "No input file given"
      | Some fn -> open_in fn, fn
    in
    Printf.printf "%s\n%!" (fn ch filename !mode)
  with
  | Failure s | Sys_error s -> Printf.eprintf "%s\n%!" s
  | e -> Printf.eprintf "An error occurer: %s\n" (Printexc.to_string e)
