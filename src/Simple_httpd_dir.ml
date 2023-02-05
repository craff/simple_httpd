module S = Simple_httpd_server
module U = Simple_httpd_util
module D = Simple_httpd_domain
module Html = Simple_httpd_html
module Pf = Printf
module Mutex = D.Mutex

type dir_behavior =
  | Index | Lists | Index_or_lists | Forbidden

type hidden = unit

type cache = NoCache | SimpleCache
             | ZlibCache of { chk : 'a . 'a S.Request.t -> bool
                            ; cmp : string -> string }

type config = {
  mutable download: bool;
  mutable dir_behavior: dir_behavior;
  mutable delete: bool;
  mutable upload: bool;
  mutable max_upload_size: int;
  mutable cache: cache;
  _rest: hidden
}

let default_config_ : config =
  { download=true;
    dir_behavior=Forbidden;
    delete=false;
    upload=false;
    max_upload_size = 10 * 1024 * 1024;
    cache=NoCache;
    _rest=();
  }

let default_config () = default_config_
let config
    ?(download=default_config_.download)
    ?(dir_behavior=default_config_.dir_behavior)
    ?(delete=default_config_.delete)
    ?(upload=default_config_.upload)
    ?(max_upload_size=default_config_.max_upload_size)
    ?(cache=default_config_.cache)
    () : config =
  { download; dir_behavior; delete; upload; max_upload_size;
    cache; _rest=()}

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

let header_html = "Content-Type", "text/html"
let (//) = Filename.concat

let encode_path s = U.percent_encode ~skip:(function '/' -> true|_->false) s
let _decode_path s = match U.percent_decode s with Some s->s | None -> s

let is_hidden s = String.length s>0 && s.[0] = '.'

module type VFS = sig
  val descr : string
  val is_directory : string -> bool
  val contains : string -> bool
  val list_dir : string -> string array
  val delete : string -> unit
  val create : string -> (bytes -> int -> int -> unit) * (unit -> unit)
  val read_file_content : string -> string
  val read_file_stream : string -> Simple_httpd_stream.t
  val file_size : string -> int option
  val file_mtime : string -> float option
end

type vfs = (module VFS)

let vfs_of_dir (top:string) : vfs =
  let module M = struct
    let descr = top
    let (//) = Filename.concat
    let is_directory f = Sys.is_directory (top // f)
    let contains f = Sys.file_exists (top // f)
    let list_dir f = Sys.readdir (top // f)
    let read_file_content f =
      let f = top // f in
      let open Unix in
      let stats = stat f in
      if stats.st_kind <> S_REG then raise Not_found;
      let ch = open_in f in
      let buf = Bytes.make stats.st_size ' ' in
      try
        really_input ch buf 0 stats.st_size;
        close_in ch;
        Bytes.unsafe_to_string buf
      with
        e -> close_in ch; raise e
    let read_file_stream f =
      let ic = Unix.(openfile (top // f) [O_RDONLY] 0) in
      (* Remark: epoll is illegal on regular file, can not
         use of_client_fd*)
      Simple_httpd_stream.of_fd ic
    let create f =
      let oc = open_out_bin (top // f) in
      let write = output oc in
      let close() = close_out oc in
      write, close
    let delete f = Sys.remove (top // f)
    let file_size f =
      try Some (Unix.stat (top // f)).Unix.st_size
      with _ -> None
    let file_mtime f =
      try Some (Unix.stat (top // f)).Unix.st_mtime
      with _ -> None
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
          match VFS.file_size fpath with
          | Some f -> Printf.sprintf " (%s)" @@ human_size f
          | None -> ""
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
    sub_e @@ h2[][txtf "Index of %S" d];
    begin match parent with
      | None -> sub_empty
      | Some p ->
        sub_e @@
        a[A.href (encode_path ("/" // prefix // p))][txt"(parent directory)"]
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
let add_vfs_ ?(accept=(fun _req x -> x)) ~config
               ~vfs:((module VFS:VFS) as vfs) ~prefix server : unit=
  let search_cache : (bool * string -> S.Response.t)
                     -> bool * string -> S.Response.t =
    if config.cache = NoCache then (fun fn key -> fn key)
    else
      begin
        let cache_mutex = Mutex.create () in
        let cache = Hashtbl.create 128 in
        let tmp_rep = S.Response.make_raw ~code:400 "" in
        fun fn (_,filename as key) ->
        try
          let (ready, mtime, ptr) = Hashtbl.find cache key in
          Mutex.wait_bool ready;
          let mtime' = VFS.file_mtime filename in
          if mtime' <> mtime then raise Not_found;
          !ptr
        with Not_found ->
           let ready = Atomic.make false in
           let mtime = VFS.file_mtime filename in
           let ptr = ref tmp_rep in
           Mutex.lock cache_mutex;
           Hashtbl.replace cache key (ready, mtime, ptr);
           Mutex.unlock cache_mutex;
           let x = fn key in
           Mutex.lock cache_mutex;
           ptr := x;
           Atomic.set ready true;
           Mutex.unlock cache_mutex;
           x
      end
  in
  let route () =
    if prefix="" then S.Route.rest_of_path_urlencoded
    else S.Route.exact_path prefix S.Route.rest_of_path_urlencoded
  in
  if config.delete then (
    S.add_route_handler ~accept server ~meth:`DELETE (route())
      (fun path _req ->
         if contains_dot_dot path then (
           U.debug ~lvl:2 (fun k->k "delete fails %s (dotdot)" path);
           S.Response.fail_raise ~code:403 "invalid path in delete"
         ) else (
           S.Response.make_string
             (try
                U.debug ~lvl:2 (fun k->k "done delete %s" path);
                VFS.delete path; "file deleted successfully"
              with e ->
                U.debug ~lvl:2 (fun k->k "delete fails %s (%s)" path
                                         (D.printexn e));
                S.Response.fail_raise ~code:500
                  "delete fails: %s (%s)" path (D.printexn e))
      )))
    else (
      S.add_route_handler ~accept server ~meth:`DELETE (route())
        (fun _ _  ->
          S.Response.fail_raise ~code:405 "delete not allowed");
    );

  if config.upload then (
    S.add_route_handler_stream server ~meth:`PUT (route())
      ~accept:(fun req ->
          match S.Request.get_header_int req "Content-Length" with
          | Some n when n > config.max_upload_size ->
             S.Response.fail_raise ~code:403
               "max upload size is %d" config.max_upload_size
          | Some _ when contains_dot_dot req.S.Request.path ->
             S.Response.fail_raise ~code:403 "invalid path (contains '..')"
          | _ -> accept req
        )
      (fun path req ->
         let write, close =
           try VFS.create path
           with e ->
             U.debug ~lvl:2 (fun k->k "fail uploading %s (%s)"
                                      path (D.printexn e));
             S.Response.fail_raise ~code:403 "cannot upload to %S: %s"
               path (D.printexn e)
         in
         let req = S.Request.limit_body_size ~max_size:config.max_upload_size req in
         Simple_httpd_stream.iter write req.S.Request.body;
         close ();
         U.debug ~lvl:2 (fun k->k "done uploading %s" path);
         S.Response.make_raw ~code:201 "upload successful"
      )
  ) else (
    S.add_route_handler ~accept server ~meth:`PUT (route())
      (fun _ _  -> S.Response.make_raw ~code:405 "upload not allowed");
  );

  if config.download then (
    S.add_route_handler ~accept server ~meth:`GET (route())
      (fun path req ->
        let mtime = lazy (
                        match VFS.file_mtime path with
                        | None -> S.Response.fail_raise ~code:403 "Cannot access file"
                        | Some t -> Printf.sprintf "mtime: %.4f" t
                      ) in
        if contains_dot_dot path then (
          U.debug ~lvl:2 (fun k->k "download fails %s (dotdot)" path);
          S.Response.fail ~code:403 "Path is forbidden";
        ) else if not (VFS.contains path) then (
          U.debug ~lvl:2 (fun k->k "download fails %s (not found)" path);
          S.Response.fail ~code:404 "File not found";
        ) else if S.Request.get_header req "If-None-Match" = Some (Lazy.force mtime) then (
          S.Response.make_raw ~code:304 ""
        ) else if VFS.is_directory path then (
          let parent = Filename.(dirname path) in
          let parent = if Filename.basename path <> "." then Some parent else None in
          match config.dir_behavior with
            | Index | Index_or_lists when VFS.contains (path // "index.html") ->
               (* redirect using path, not full path *)
               let new_path = "/" // prefix // path // "index.html" in
               U.debug ~lvl:2 (fun k->k "download redirect %s" path);
               S.Response.make_raw ~code:301 ""
                 ~headers:S.Headers.(empty |> set "location" new_path)
            | Lists | Index_or_lists ->
               let body = html_list_dir ~prefix vfs path ~parent
                          |> Html.to_string_top in
               U.debug ~lvl:2 (fun k->k "download index %s" path);
               S.Response.make_string
                 ~headers:[header_html; "ETag", Lazy.force mtime]
                 body
            | Forbidden | Index ->
               U.debug ~lvl:2 (fun k->k "download index fails %s (forbidden)" path);
               S.Response.make_raw ~code:405 "listing dir not allowed"
        ) else (
          let deflate = match config.cache with
            | ZlibCache {chk; _} -> chk req
            | _ -> false
          in
          let fn (deflate,path) =
            try
              let mime_type =
                ["Content-Type", Magic_mime.lookup path]
              in
              match config.cache with
              | ZlibCache {cmp; _} when deflate ->
                 let string = cmp (VFS.read_file_content path) in
                 U.debug ~lvl:2 (fun k->k "download ok %s" path);
                 S.Response.make_raw_chunked
                   ~headers:(mime_type@[("Etag", Lazy.force mtime)
                                       ;("content-encoding", "chunked, deflate")])
                   ~code:200 (Simple_httpd_stream.string_to_chunk string)
              | SimpleCache | ZlibCache _ ->
                 let string = VFS.read_file_content path in
                 U.debug ~lvl:2 (fun k->k "download ok %s" path);
                 if String.length string <= 50_000 then
                   S.Response.make_raw
                     ~headers:(mime_type@[("Etag", Lazy.force mtime)])
                     ~code:200 string
                 else
                   S.Response.make_raw_chunked
                     ~headers:(mime_type@[("Etag", Lazy.force mtime)
                                         ;("content-encoding", "chunked")])
                     ~code:200 (Simple_httpd_stream.string_to_chunk string)
              | NoCache ->
                 let stream = VFS.read_file_stream path in
                 U.debug ~lvl:2 (fun k->k "download ok %s" path);
                 S.Response.make_raw_stream
                   ~headers:(mime_type@[("Etag", Lazy.force mtime)])
                   ~code:200 stream
            with e ->
              U.debug ~lvl:2 (fun k->k "download fails %s (%s)" path (D.printexn e));
              S.Response.fail ~code:500 "error while reading file: %s" (D.printexn e)
          in
          search_cache fn (deflate,path)
        )
      )
  ) else (
    S.add_route_handler server ~accept ~meth:`GET (route())
      (fun _ _  -> S.Response.make_raw ~code:405 "download not allowed");
  );
  ()

let add_vfs ?accept ~config ~vfs ~prefix server : unit =
  add_vfs_ ?accept ~config ~prefix ~vfs server

let add_dir_path ?accept ~config ~dir ~prefix server : unit =
  add_vfs_ ?accept ~config ~prefix ~vfs:(vfs_of_dir dir) server

module Embedded_fs = struct
  module Str_map = Map.Make(String)

  type t = {
    mtime: float;
    mutable entries: entry Str_map.t
  }

  and entry =
    | File of {
        content: string;
        mtime: float;
      }
    | Dir of t

  let create ?(mtime=Unix.gettimeofday()) () : t = {
    mtime;
    entries=Str_map.empty;
  }

  let split_path_ (path:string) : string list * string =
    let basename = Filename.basename path in
    let dirname =
      Filename.dirname path
      |> String.split_on_char '/'
      |> List.filter (function "" | "." -> false | _ -> true) in
    dirname, basename

  let add_file ?mtime (self:t) ~path content : unit =
    let mtime = match mtime with Some t -> t | None -> self.mtime in
    let dir_path, basename = split_path_ path in
    if List.mem ".." dir_path then (
      invalid_arg "add_file: '..' is not allowed";
    );

    let rec loop self dir = match dir with
      | [] ->
        self.entries <- Str_map.add basename (File {mtime; content}) self.entries
      | d :: ds ->
        let sub =
          match Str_map.find d self.entries with
          | Dir sub -> sub
          | File _ ->
            invalid_arg
              (Printf.sprintf "in path %S, %S is a file, not a directory" path d)
          | exception Not_found ->
            let sub = create ~mtime:self.mtime () in
            self.entries <- Str_map.add d (Dir sub) self.entries;
            sub
        in
        loop sub ds
    in
    loop self dir_path

  (* find entry *)
  let find_ self path : entry option =
    let dir_path, basename = split_path_ path in
    let rec loop self dir_name = match dir_name with
      | [] -> (try Some (Str_map.find basename self.entries) with _ -> None)
      | d :: ds ->
        match Str_map.find d self.entries with
        | exception Not_found -> None
        | File _ -> None
        | Dir sub -> loop sub ds
    in
    if path="" then Some (Dir self)
    else loop self dir_path

  let to_vfs self : vfs =
    let module M = struct
      let descr = "Embedded_fs"
      let file_mtime p = match find_ self p with
        | Some (File {mtime;_}) -> Some mtime
        | Some (Dir _) -> Some self.mtime
        | _ -> None

      let file_size p = match find_ self p with
        | Some (File {content;_}) -> Some (String.length content)
        | _ -> None

      let contains p = match find_ self p with
        | Some _ -> true
        | None -> false

      let is_directory p = match find_ self p with
        | Some (Dir _) -> true
        | _ -> false

      let read_file_content p = match find_ self p with
        | Some (File {content;_}) -> content
        | _ -> failwith (Printf.sprintf "no such file: %S" p)

      let read_file_stream p = match find_ self p with
        | Some (File {content;_}) -> Simple_httpd_stream.of_string content
        | _ -> failwith (Printf.sprintf "no such file: %S" p)

      let list_dir p = match find_ self p with
        | Some (Dir sub) ->
          Str_map.fold (fun sub _ acc -> sub::acc) sub.entries [] |> Array.of_list
        | _ -> failwith (Printf.sprintf "no such directory: %S" p)

      let create _ = failwith "Embedded_fs is read-only"
      let delete _ = failwith "Embedded_fs is read-only"

    end in (module M)

end
