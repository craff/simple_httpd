let spf = Printf.sprintf
let fpf = Printf.fprintf
let now_ = Unix.gettimeofday()
let verbose = ref false
let max_size = ref 0x8000
let exclude = ref []

type entry =
  | File of bool * string * string
  | Url of string * string
  | Path of bool * string * string * string
  | Mirror of string * string
  | MlHtml of string * string

let read_file filename =
  let ic = open_in_bin filename in
  let buf = Buffer.create 32 in
  let b = Bytes.create 1024 in
  while
    let n=input ic b 0 (Bytes.length b) in
    Buffer.add_subbytes buf b 0 n;
    n > 0
  do () done;
  close_in ic;
  Buffer.contents buf

let log k =
  if !verbose then k (Printf.kfprintf (fun ch -> Printf.fprintf ch "\n%!") stderr)

let split_comma s = Scanf.sscanf s "%s@,%s" (fun x y -> x,y)

let ( // ) = Filename.concat

let is_url s =
  let is_prefix pre s =
    String.length s > String.length pre &&
    String.sub s 0 (String.length pre) = pre
  in
  is_prefix "http://" s || is_prefix "https://" s

(* I using path, destination will contain only those file,
   raise Failure if path is used and destination is not provided *)

let emit ~perm ?max_size ?destination oc (l:entry list) : unit =
  let dest = match destination with
    | Some d -> d
    | None -> ""
  in
  fpf oc "open Simple_httpd
          let make ?(top_dir=%S) () =
          let module M = struct
          let embedded_fs = Dir.Embedded_fs.create
                               ~top:top_dir ~mtime:%f ()\n" dest now_;

  let add_vfs vfs_path ~mtime ~mime content =
    fpf oc
      "let () = (Dir.Embedded_fs.add_file embedded_fs \n  \
       ~mtime:%h ~headers:[Headers.Content_Type, %S]  ~path:%S)\n  \
       %S\n"
      mtime mime vfs_path content
  in

  let add_vfs_path vfs_path ~mtime ~mime zpath =
    match zpath with
    | None ->
       fpf oc
         "let () = (Dir.Embedded_fs.add_path embedded_fs \n  \
          ~path:%S ~mtime:%h ~headers:[Headers.Content_Type, %S] (Filename.concat top_dir %S))\n"
         vfs_path mtime mime vfs_path
    | Some zpath ->
       fpf oc
         "let () = (Dir.Embedded_fs.add_path embedded_fs \n  \
          ~path:%S ~mtime:%h ~headers:[Headers.Content_Type, %S] ~deflate:(Filename.concat top_dir %S)
          (Filename.concat top_dir %S))\n"
         vfs_path mtime mime zpath vfs_path
  in

  let rec add_entry = function
    | File (html, vfs_path, actual_path) ->
      log (fun k -> k  "add file %S = %S" vfs_path actual_path);
      begin
        let content =
          if html then
            begin
              let ch = open_in actual_path in
              let filename = actual_path in
              log (fun k -> k "parsing %s" filename);
              Parser.document_parse ~filename ch
            end
          else
            read_file actual_path
        in
        let mtime = (Unix.stat actual_path).Unix.st_mtime in
        let mime =  Magic_mime.lookup actual_path in
        add_vfs ~mtime ~mime vfs_path content
      end

    | Path (_html, vfs_path, actual_path, store_path) ->
       let actual_path = actual_path // vfs_path in
       let disk_path = store_path // vfs_path in
       log (fun k -> k "add path %S = %S in %S" vfs_path actual_path disk_path);
       if disk_path <> actual_path then
         if Sys.command (spf "install -C -m %o \"%s\" \"%s\"" (perm land 0o666) actual_path disk_path) <> 0
         then failwith
                (spf "vfs_pack: can not copy path %s to %s" actual_path disk_path);
       let stats = Unix.stat actual_path in
       let zpath =
         let zpath = disk_path ^".zlib" in
         (try Simple_httpd.Camlzip.file_deflate disk_path zpath with _ -> ());
         let statsz = Unix.stat zpath in
         if float statsz.st_size > 0.9 *. float stats.st_size then
           (Sys.remove zpath; None)
         else
           (Some (vfs_path ^ ".zlib"))
       in
       let mime =  Magic_mime.lookup disk_path in
       let mtime = stats.st_mtime in
       add_vfs_path vfs_path ~mtime ~mime zpath

    | MlHtml(vfs_path, actual_path) ->
       log (fun k -> k "add mlhtml %S = %S" vfs_path actual_path);
       let ch = open_in actual_path in
       let filename = actual_path in
       let modulename =
         String.capitalize_ascii (Filename.chop_extension (Filename.basename filename))
       in
       let (ml, globals, prelude) = Parser.chaml_parse ~filename ch in
       fpf oc
         "module %s = struct %s end \n  \
          let () = (Dir.Embedded_fs.add_dynamic embedded_fs \n  \
          ~path:%S ~headers:[Headers.Content_Type, \"text/html\"]
          (fun [@warning \"-27\"] request headers ->
          let module M = struct
          open %s  [@@warning \"-33\"]
          let [@warning \"-32\"] cookies = Cookies.empty
          %s
          end in let open [@warning \"-33\"] M in let open [@warning \"-33\"] %s in
          let input =
          Input.of_output (fun (module Out : Html.Output) ->
          let open Out [@warning \"-33\"] in
          let module M = struct %s end in
          ()) in
            ( headers, cookies, input )))\n%!"
         modulename globals vfs_path
         modulename prelude
         modulename ml

    | Url (vfs_path, url) ->
      log (fun k -> k "add url %S = %S" vfs_path url);

      begin match Curly.get ~args:["-L"] url with
        | Ok b ->
           let code = b.Curly.Response.code in
           let mime =  Magic_mime.lookup vfs_path in

           if code >= 200 && code < 300 then (
             add_vfs ~mtime:now_ ~mime vfs_path b.Curly.Response.body
          ) else (
            failwith (spf "download of %S failed with code: %d" url code)
          )
        | Error err ->
          failwith (Format.asprintf "download of %S failed: %a" url Curly.Error.pp err)
      end

    | Mirror (vfs_path, dir) ->
      log (fun k -> k "mirror directory %S as %S" dir vfs_path);
      let store = match destination with
        | None -> dir
        | Some d -> d
      in
      let rec traverse vfs_path =
        let real_path = dir // vfs_path in
        let store_path = store // vfs_path in
        if List.mem vfs_path !exclude then () else
        if Sys.is_directory real_path then (
          if not (Sys.file_exists store_path && Sys.is_directory store_path) then
            Sys.mkdir store_path perm;
          let arr = Sys.readdir real_path in
          Array.iter (fun e -> traverse (vfs_path // e)) arr
        ) else (
          let basename = Filename.basename vfs_path in
          let lenbase = String.length basename in
          if lenbase > 0 && basename.[0] <> '.' && basename.[0] <> '#'
             && basename.[lenbase - 1] <> '~'
          then
            begin
              let extension = Filename.extension vfs_path in
              if extension = ".chaml" then
                let vpath = Filename.remove_extension vfs_path ^ ".html" in
                add_entry (MlHtml (vpath, real_path))
              else if extension <> ".zlib" && extension <> ".htinc" then
                begin
                  let use_path =
                    match max_size with
                    | None -> false
                    | Some s -> (Unix.stat real_path).st_size > s
                  in
                  let html = (extension = ".html") in
                  if use_path then
                    add_entry (Path (html, vfs_path, dir, store))
                  else
                    add_entry (File (html, vfs_path, real_path))
                end
            end
        )
      in
      traverse ""

  in
  List.iter add_entry l;

  fpf oc "let vfs = Dir.Embedded_fs.to_vfs embedded_fs\nend in\n
          M.vfs";
  ()


let help = {|vfs-pack [opt]+

Builds an OCaml module containing a `Simple_httpd.Dir.Embedded_fs.t`
virtual file system. This is useful to pack assets into an OCaml binary,
for example.

Each entry in the VFS can be added from the command line.
|}


let () =
  let entries = ref [] in
  let out = ref "" in
  let destination = ref None in
  let perm = ref 0o700 in

  let add_entry e = entries := e :: !entries in

  let add_file s =
    let vfs_path, path = split_comma s in
    let html = (Filename.extension vfs_path = ".html") in
    add_entry (File (html, vfs_path, path))
  and add_mirror s =
    let vfs_path, path = split_comma s in
    let vfs_path, path = if path="" then "", vfs_path else vfs_path, path in
    add_entry (Mirror (vfs_path, path))
  and add_url s =
    let vfs_path, path = split_comma s in
    if is_url path then add_entry (Url(vfs_path, path))
    else invalid_arg (spf "--url: invalid URL %S" path)
  in

  let rec opts = lazy ([
    "-v", Arg.Set verbose, " verbose mode";
    "-o", Arg.Set_string out, " <file> set output file";
    "--file", Arg.String add_file, " <name,file> adds name=file to the VFS";
    "--url", Arg.String add_url, " <name,url> adds name=url to the VFS";
    "--mirror", Arg.String add_mirror, " <prefix,dir> adds prefix=dir to the vfs, copying all files in directory dir";
    "--max-size", Arg.Set_int max_size, " <size>, max size to hold file in memory (default: infinite). Bigger filed are copied to the folder given by --destination. A compressed version .zlib is also produced if it is at least 10% smaller.";
    ("--destination", Arg.String (fun s -> destination := Some s),
     " <dir> set the destination folder to use with mirror (default the same as dir)");
    ("--perm", Arg.Set_int perm,
     " <int>set the permission of created folder");
    ("-F", Arg.String parse_source,
     " <file> reads entries from the file, written using this command line option syntax.");
    ("--exclude", Arg.String (fun s -> exclude := s :: !exclude),
     " <file> exclude the file or folder from the mirror")
  ] |> Arg.align)

  and parse args =
    let opts = Lazy.force opts in
    Arg.parse_argv ~current:(ref 0) args opts (fun _ -> raise (Arg.Help "no positional arg")) help

  and parse_source f =
      log (fun k -> k "read source file %S" f);
      let args = Arg.read_arg f in
      parse args
  in
  parse Sys.argv;

  let out, close =
    if !out="" then stdout,ignore
    else open_out !out, close_out
  in
  let perm = !perm and destination = !destination and max_size = !max_size in
  emit ~perm ?destination ~max_size out !entries;
  close out;
  exit 0
