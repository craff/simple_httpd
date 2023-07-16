open Response_code
module Mutex = Async.Mutex

let log = Log.f

type dir_behavior =
  | Index | Lists | Index_or_lists | Forbidden

type config = {
  mutable download: bool;
  mutable dir_behavior: dir_behavior;
  mutable delete: bool;
  mutable upload: bool;
  mutable max_upload_size: int;
}

let default_config_ : config =
  { download=true;
    dir_behavior=Forbidden;
    delete=false;
    upload=false;
    max_upload_size = 10 * 1024 * 1024;
  }

let default_config () = default_config_
let config
    ?(download=default_config_.download)
    ?(dir_behavior=default_config_.dir_behavior)
    ?(delete=default_config_.delete)
    ?(upload=default_config_.upload)
    ?(max_upload_size=default_config_.max_upload_size)
    () : config =
  { download; dir_behavior; delete; upload; max_upload_size }

let contains_dot_dot s =
  try
    String.iteri
      (fun i c ->
         if c='.' && i+1 < String.length s && String.get s (i+1) = '.' then raise Exit)
      s;
    false
  with Exit -> true

(* Human readable size *)
let human_size (x:int) : string =
  if x >= 1_000_000_000 then Printf.sprintf "%d.%dG" (x / 1_000_000_000) ((x/1_000_000) mod 1_000_000)
  else if x >= 1_000_000 then Printf.sprintf "%d.%dM" (x / 1_000_000) ((x/1000) mod 1_000)
  else if x >= 1_000 then Printf.sprintf "%d.%dk" (x/1000) ((x/100) mod 100)
  else Printf.sprintf "%db" x

let header_html = Headers.Content_Type, "text/html"
let (//) = Filename.concat

let encode_path s = Util.percent_encode ~skip:(function '/' -> true|_->false) s

let is_hidden s = String.length s>0 && s.[0] = '.'

type dynamic = string Request.t -> Headers.t -> Headers.t * Cookies.t * Input.t

type 'a content =
  | String of string * string option
  | Path   of string * (string * int) option
  | Dynamic of dynamic
  | Stream of Input.t
  | Fd of Unix.file_descr
  | Dir of 'a

type file_info =
  FI : { content : 'a content
       ; size : int option
       ; mtime : float option
       ; headers : Headers.t } -> file_info


module type VFS = sig
  val descr : string
  val is_directory : string -> bool
  val contains : string -> bool
  val list_dir : string -> string array
  val delete : string -> unit
  val create : string -> (bytes -> int -> int -> unit) * (unit -> unit)
  val read_file : string -> file_info
end

type vfs = (module VFS)

let vfs_of_dir (top:string) : vfs =
  let module M = struct
    let descr = top
    let (//) = Filename.concat
    let is_directory f = Sys.is_directory (top // f)
    let contains f = Sys.file_exists (top // f)
    let list_dir f = Sys.readdir (top // f)
    let create f =
      let oc = open_out_bin (top // f) in
      let write = output oc in
      let close() = close_out oc in
      write, close
    let delete f = Sys.remove (top // f)
    let read_file f =
      let oc = Unix.openfile (top // f) [O_RDONLY] 0 in
      let stats = Unix.fstat oc in
      let content = Fd(oc) in
      let size = if stats.st_kind = S_REG then
                   Some stats.st_size else None
      in
      let mtime = Some stats.st_mtime in
      let mime =  Magic_mime.lookup f in
      let headers = [(Headers.Content_Type, mime)] in
      FI { content; size; mtime; headers }
  end in
  (module M)

let html_list_dir (module VFS:VFS) ~prefix ~parent d : Html.elt =
  let entries = VFS.list_dir d in
  Array.sort String.compare entries;
  let open Html in

  (* TODO: breadcrumbs for the path, each element a link to the given ancestor dir *)
  let head =
    head[][
      title[][txtf "list directory %S" VFS.descr];
      meta[A.charset "utf-8"];
    ] in

  let n_hidden = ref 0 in
  Array.iter (fun f -> if is_hidden f then incr n_hidden) entries;

  let file_to_elt f : elt option =
    if not @@ contains_dot_dot (d // f) then (
      let fpath = d // f in
      if not @@ VFS.contains fpath then (
        Some (li[][txtf "%s [invalid file]" f])
      ) else (
        let size =
          try
            match VFS.read_file fpath with
            | FI { size = Some f ; _ } -> Printf.sprintf " (%s)" @@ human_size f
            | _ -> ""
          with _ -> ""
        in
        Some (li'[] [
          sub_e @@ a[A.href ("/" // prefix // fpath)][txt f];
          (if VFS.is_directory fpath then sub_e @@ txt "[dir]" else sub_empty);
          sub_e @@ txt size;
        ])
      )
    ) else None
  in

  let body = body'[] [
    sub_e @@ h2[][txtf "Index of %S" (prefix // d)];
    begin match parent with
      | None -> sub_empty
      | Some p ->
        sub_e @@
        a[A.href (encode_path ("/" // p))][txt"(parent directory)"]
    end;

    sub_e @@ ul' [] [
      if !n_hidden>0 then
        sub_e @@ details'[][
          sub_e @@ summary[][txtf "(%d hidden files)" !n_hidden];
          sub_seq (
            seq_of_array entries
            |> Seq.filter_map
              (fun f -> if is_hidden f then file_to_elt f else None)
          );
        ] else sub_empty;
      sub_seq (
        seq_of_array entries
        |> Seq.filter_map (fun f ->
            if not (is_hidden f) then file_to_elt f else None)
      )
    ];
  ]
  in
  html [][head; body]

(* @param on_fs: if true, we assume the file exists on the FS *)
let add_vfs_ ?addresses ?hostnames ?(filter=(fun x -> (x, fun r -> r)))
               ?(config=default_config ())
               ?(prefix="") ~vfs:((module VFS:VFS) as vfs) server : unit=
  let route () =
    if prefix="" then Route.rest
    else Route.exact_path prefix Route.rest
  in
  let check must_exists ope path =
    let path = String.concat "/" path in
    if contains_dot_dot path then (
      log (Exc 0) (fun k->k "%s fails %s (dotdot)" ope path);
      Response.fail_raise ~code:forbidden "Path is forbidden");
    if must_exists && not (VFS.contains path) then Route.pass ();
    path
  in
  if config.delete then (
    Server.add_route_handler ?addresses ?hostnames ~filter ~meth:DELETE
      server (route())
      (fun path -> let path = check true "delete" path in fun _req ->
           Response.make_string
             (try
                log (Req 1) (fun k->k "done delete %s" path);
                VFS.delete path; "file deleted successfully"
              with e ->
                log (Exc 0) (fun k->k "delete fails %s (%s)" path
                                         (Async.printexn e));
                Response.fail_raise ~code:internal_server_error
                  "delete fails: %s (%s)" path (Async.printexn e))
      ))
    else (
      Server.add_route_handler ?addresses ?hostnames ~filter ~meth:DELETE server (route())
        (fun _ _  ->
          Response.fail_raise ~code:method_not_allowed "delete not allowed");
    );

  if config.upload then (
    Server.add_route_handler_stream ?addresses ?hostnames ~meth:PUT server (route())
      ~filter:(fun req ->
          match Request.get_header_int req Headers.Content_Length with
          | Some n when n > config.max_upload_size ->
             Response.fail_raise ~code:forbidden
               "max upload size is %d" config.max_upload_size
          | Some _ when contains_dot_dot req.Request.path ->
             Response.fail_raise ~code:forbidden "invalid path (contains '..')"
          | _ -> filter req
        )
      (fun path -> let path = check false "upload" path in fun req ->
         let write, close =
           try VFS.create path
           with e ->
             log (Exc 0) (fun k->k "fail uploading %s (%s)"
                                      path (Async.printexn e));
             Response.fail_raise ~code:forbidden "cannot upload to %S: %s"
               path (Async.printexn e)
         in
         let req = Request.limit_body_size ~max_size:config.max_upload_size req in
         Input.iter write req.Request.body;
         close ();
         log (Req 1) (fun k->k "done uploading %s" path);
         Response.make_raw ~code:created "upload successful"
      )
  ) else (
    Server.add_route_handler ?addresses ?hostnames ~filter ~meth:PUT server (route())
      (fun _ _  -> Response.make_raw ~code:method_not_allowed
                     "upload not allowed");
  );

  if config.download then (
    Server.add_route_handler ?addresses ?hostnames ~filter ~meth:GET server (route())
      (fun path -> let path = check true "download" path in fun req ->
        let FI info = VFS.read_file path in
        let mtime =  match info.mtime with
          | None -> None
          | Some t -> Some (Printf.sprintf "mtime: %.4f" t)
        in
        let may_cache () =
          mtime <> None && Request.get_header req Headers.If_None_Match = mtime
        in
        if may_cache () then Response.make_raw ~code:not_modified "" else
        let cache_control () =
          match mtime with
          | None -> (Headers.Cache_Control, "no-store")
          | Some mtime -> (Headers.ETag, mtime)
        in
        if VFS.is_directory path then (
          let parent = Some (Filename.(dirname (prefix // path))) in
          match config.dir_behavior with
            | Index | Index_or_lists when VFS.contains (path // "index.html") ->
               (* redirect using path, not full path *)
               let new_path = "/" // prefix // path // "index.html" in
               let query = String.concat "&"
                             (List.map (fun (k,v) -> k ^ "=" ^ v)
                                (Request.query req)) in
               Response.make_raw ~code:moved_permanently "moved"
                 ~headers:Headers.(empty
                                   |> set Headers.Location (new_path ^ "?" ^ query)
                                   |> set Headers.Content_Type "text/plain")
            | Lists | Index_or_lists ->
               let body = html_list_dir ~prefix vfs path ~parent
                          |> Html.to_string_top in
               log (Req 1) (fun k->k "download index %s" path);
               Response.make_string ~headers:[header_html] body
            | Forbidden | Index ->
               Response.make_raw ~code:forbidden "listing dir not allowed"
        ) else (
          let accept_encoding =
            match Request.(get_header req Headers.Accept_Encoding)
            with None -> []
               | Some l -> List.map String.trim (String.split_on_char ',' l)
          in
          let deflate = List.mem "deflate" accept_encoding in
          match info.content with
          | Path(_, Some (fz, size)) when deflate ->
             let fd = Unix.openfile fz [O_RDONLY] 0 in
             Response.make_raw_file
               ~headers:(cache_control ()::
                         (Headers.Content_Encoding, "deflate")::info.headers)
               ~code:ok ~close:true size fd
          | Path(f, _) ->
             let fd = Unix.openfile f [O_RDONLY] 0 in
             let size = match info.size with Some s -> s | None -> assert false in
             Response.make_raw_file
               ~headers:(cache_control ()::info.headers)
               ~code:ok ~close:true size fd
          | Fd(fd) ->
             let size = Unix.(fstat fd).st_size in
             Response.make_raw_file
               ~headers:(cache_control ()::info.headers)
               ~code:ok ~close:true size fd
          | String(_, Some sz) when deflate ->
             Response.make_raw
               ~headers:(cache_control ()::
                         (Headers.Content_Encoding, "deflate")::info.headers)
               ~code:ok sz
          | String(s, _) ->
             Response.make_raw
               ~headers:(cache_control ()::info.headers)
               ~code:ok s
          | Dynamic f ->
             let headers = [cache_control ()] in
             let headers, cookies, input = f req headers in
             Response.make_raw_stream
               ~headers ~cookies ~code:ok input
          | Stream input ->
             Response.make_raw_stream
               ~headers:(cache_control ()::info.headers)
               ~code:ok input

          | Dir _ -> assert false

        )
      )
  ) else (
    Server.add_route_handler ?addresses ?hostnames ~filter ~meth:GET server (route())
      (fun _ _  -> Response.make_raw ~code:method_not_allowed "download not allowed");
  );
  ()

let add_vfs ?addresses ?hostnames ?filter ?prefix ?config ~vfs server : unit =
  add_vfs_ ?addresses ?hostnames ?filter ?prefix ?config ~vfs server

let add_dir_path ?addresses ?hostnames ?filter ?prefix ?config ~dir server : unit =
  add_vfs_ ?addresses ?hostnames ?filter ?prefix ?config ~vfs:(vfs_of_dir dir) server

module Embedded_fs = struct

  type t = {
    emtime: float;
    entries: (string,entry) Hashtbl.t;
    top : string
  }

  and entry = {
      mtime : float option;
      mutable size: int option;
      kind : kind;
      headers: Headers.t
    }

  and kind = t content

  let create ?(top="") ?(mtime=Unix.gettimeofday()) () : t = {
    emtime=mtime;
    entries=Hashtbl.create 128;
    top;
    }

  let split_path_ (path:string) : string list =
    String.split_on_char '/' path

  let add_file_gen (self:t) ~path content : unit =
    let dir_path = split_path_ path in
    if List.mem ".." dir_path then (
      invalid_arg "add_file: '..' is not allowed";
    );

    let rec loop (self:t) dir = match dir with
      | [] -> assert false
      | [basename] ->
         Hashtbl.replace self.entries basename content
      | "." :: ds -> loop self ds
      | d :: ds ->
        let sub =
          match (Hashtbl.find self.entries d).kind with
          | Dir sub -> sub
          | _ ->
            invalid_arg
              (Printf.sprintf "in path %S, %S is a file, not a directory" path d)
          | exception Not_found ->
             let sub = create ~mtime:self.emtime () in
             let entry =
               { kind = Dir sub; mtime = Some self.emtime;
                 size = None; headers = [] }
             in
             Hashtbl.add self.entries d entry;
             sub
        in
        loop sub ds
    in
    loop self dir_path

  let add_file (self:t) ~path ?mtime ?(headers=[]) content : unit =
    let mtime = match mtime with Some t -> t | None -> self.emtime in
    let size = String.length content in
    let sz = Camlzip.deflate_string content in
    let sz =
      if float (String.length sz) > 0.9 *. float size then
        None else Some sz
    in
    let kind = String(content, sz) in
    let entry = { mtime = Some mtime; headers; size = Some size; kind } in
    add_file_gen (self:t) ~path entry

  let add_dynamic (self:t) ~path ?mtime ?(headers=[]) content : unit =
    let entry = { mtime; headers; size = None; kind = Dynamic content} in
    add_file_gen (self:t) ~path entry

  let add_path (self:t) ~path ?mtime ?(headers=[]) ?deflate rpath : unit =
    (*let fz = rpath ^".zlib" in *)
    let deflate = Option.map (fun x ->
                      let size = (Unix.stat x).st_size in
                      x, size) deflate in
    let content = Path(rpath, deflate) in
    let size = Some (Unix.stat rpath).st_size in
    let entry = { mtime; headers; size; kind = content} in
    add_file_gen (self:t) ~path entry

  (* find entry *)
  let find_ self path : entry option =
    let dir_path = split_path_ path in
    let rec loop self dir_name = match dir_name with
      | [] -> assert false
      | [basename] -> (try Some (Hashtbl.find self.entries basename) with _ -> None)
      | "." :: ds -> loop self ds
      | d :: ds ->
        match (Hashtbl.find self.entries d).kind with
        | Dir sub -> loop sub ds
        | _ -> None
        | exception Not_found -> None
    in
    if path="" then Some { mtime = Some self.emtime;
                           size = None;
                           kind = Dir self;
                           headers =[] }
    else loop self dir_path

  let to_vfs self : vfs =
    let module M = struct
      let descr = "Embedded_fs"

      let read_file p =
        match find_ self p with
        | Some { mtime; headers; kind = content; size } ->
           FI { content; mtime; size; headers }
        | _ -> Response.fail_raise ~code:not_found "File %s not found" p

      let contains p = match find_ self p with
        | Some _ -> true
        | None -> false

      let is_directory p = match find_ self p with
        | Some { kind = Dir _; _ } -> true
        | _ -> false

      let list_dir p = match find_ self p with
        | Some { kind = Dir sub; _ } ->
          Hashtbl.fold (fun sub _ acc -> sub::acc) sub.entries [] |> Array.of_list
        | _ -> failwith (Printf.sprintf "no such directory: %S" p)

      let create _ = failwith "Embedded_fs is read-only"
      let delete _ = failwith "Embedded_fs is read-only"

    end in (module M)

end
