module S = Simple_httpd_server
module U = Simple_httpd_util
module D = Simple_httpd_domain
module H = S.Headers
module Html = Simple_httpd_html
module Pf = Printf
module Mutex = D.Mutex
module Input = Simple_httpd_input

type dir_behavior =
  | Index | Lists | Index_or_lists | Forbidden

type cache = NoCache
           | MemCache
           | CompressCache of string * (string -> string)
           | SendFile
           | SendFileCache


type cache_key =
           | MemCacheKey
           | CompressCacheKey of string
           | SendFileCacheKey

let cache_key = function
  | NoCache | SendFile -> assert false
  | MemCache -> MemCacheKey
  | CompressCache (s,_) -> CompressCacheKey s
  | SendFileCache -> SendFileCacheKey

type choose_cache = size:int option -> mime:string -> accept_encoding:string list
                    -> cache

type config = {
  mutable download: bool;
  mutable dir_behavior: dir_behavior;
  mutable delete: bool;
  mutable upload: bool;
  mutable max_upload_size: int;
  mutable cache: choose_cache;
}

let default_config_ : config =
  { download=true;
    dir_behavior=Forbidden;
    delete=false;
    upload=false;
    max_upload_size = 10 * 1024 * 1024;
    cache=(fun ~size:_ ~mime:_ ~accept_encoding:_ -> NoCache);
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
    cache; }

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

let header_html = H.Content_Type, "text/html"
let (//) = Filename.concat

let encode_path s = U.percent_encode ~skip:(function '/' -> true|_->false) s

let is_hidden s = String.length s>0 && s.[0] = '.'

module type VFS = sig
  val descr : string
  val is_directory : string -> bool
  val contains : string -> bool
  val list_dir : string -> string array
  val delete : string -> unit
  val create : string -> (bytes -> int -> int -> unit) * (unit -> unit)
  val read_file_content : string -> string
  val read_file_stream : string -> Simple_httpd_input.t
  val read_file_fd : string -> int * Unix.file_descr
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
      let fd = Unix.(openfile (top // f) [O_RDONLY] 0) in
      Input.of_fd fd
      (* Remark: epoll is illegal on regular file, can not
         use of_client_fd*)
    let read_file_fd f =
      let open Unix in
      let stat = stat (top // f) in
      let fd = openfile (top // f) [O_RDONLY] 0 in
      (stat.st_size, fd)
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
let add_vfs_ ?(filter=(fun x -> (x, fun r -> r))) ~config
               ~vfs:((module VFS:VFS) as vfs) ~prefix server : unit=
  let search_cache : (string -> S.Response.t)
                     -> cache_key * string -> S.Response.t =
    let cache_mutex = Mutex.create () in
    let cache = Hashtbl.create 128 in
    let tmp_rep = S.Response.make_raw ~code:400 "" in
    fun fn (_,filename as key) ->
    try
      let (ready, mtime, ptr) = Hashtbl.find cache key in
      while not (Atomic.get ready) do D.sleep 0.01; done; (* FIXME *)
      let mtime' = VFS.file_mtime filename in
      if mtime' <> mtime then
        begin
          (match (!ptr).S.Response.body with
           | File(_,fd,_) -> (try Unix.close fd with _ -> ())
           | _ -> ());
          raise Not_found;
        end;
      !ptr
    with Not_found ->
      let ready = Atomic.make false in
      let mtime = VFS.file_mtime filename in
      let ptr = ref tmp_rep in
      Mutex.lock cache_mutex;
      Hashtbl.replace cache key (ready, mtime, ptr);
      Mutex.unlock cache_mutex;
      let x = fn filename in
      Mutex.lock cache_mutex;
      ptr := x;
      Atomic.set ready true;
      Mutex.unlock cache_mutex;
      x
  in
  let route () =
    if prefix="" then S.Route.rest
    else S.Route.exact_path prefix S.Route.rest
  in
  if config.delete then (
    S.add_route_handler ~filter server ~meth:DELETE (route())
      (fun path _req ->
         let path = String.concat "/" path in
         if contains_dot_dot path then (
           D.log ~lvl:2 (fun k->k "delete fails %s (dotdot)" path);
           S.Response.fail_raise ~code:403 "invalid path in delete"
         ) else (
           S.Response.make_string
             (try
                S.log ~lvl:2 (fun k->k "done delete %s" path);
                VFS.delete path; "file deleted successfully"
              with e ->
                S.log ~lvl:2 (fun k->k "delete fails %s (%s)" path
                                         (D.printexn e));
                S.Response.fail_raise ~code:500
                  "delete fails: %s (%s)" path (D.printexn e))
      )))
    else (
      S.add_route_handler ~filter server ~meth:DELETE (route())
        (fun _ _  ->
          S.Response.fail_raise ~code:405 "delete not allowed");
    );

  if config.upload then (
    S.add_route_handler_stream server ~meth:PUT (route())
      ~filter:(fun req ->
          match S.Request.get_header_int req H.Content_Length with
          | Some n when n > config.max_upload_size ->
             S.Response.fail_raise ~code:403
               "max upload size is %d" config.max_upload_size
          | Some _ when contains_dot_dot req.S.Request.path ->
             S.Response.fail_raise ~code:403 "invalid path (contains '..')"
          | _ -> filter req
        )
      (fun path req ->
         let path = String.concat "/" path in
         let write, close =
           try VFS.create path
           with e ->
             S.log ~lvl:2 (fun k->k "fail uploading %s (%s)"
                                      path (D.printexn e));
             S.Response.fail_raise ~code:403 "cannot upload to %S: %s"
               path (D.printexn e)
         in
         let req = S.Request.limit_body_size ~max_size:config.max_upload_size req in
         Input.iter write req.S.Request.body;
         close ();
         S.log ~lvl:2 (fun k->k "done uploading %s" path);
         S.Response.make_raw ~code:201 "upload successful"
      )
  ) else (
    S.add_route_handler ~filter server ~meth:PUT (route())
      (fun _ _  -> S.Response.make_raw ~code:405 "upload not allowed");
  );

  if config.download then (
    S.add_route_handler ~filter server ~meth:GET (route())
      (fun path req ->
        let path = String.concat "/" path in
        let mtime = lazy (
                        match VFS.file_mtime path with
                        | None -> S.Response.fail_raise ~code:403 "Cannot access file"
                        | Some t -> Printf.sprintf "mtime: %.4f" t
                      ) in
        if contains_dot_dot path then (
          S.log ~lvl:2 (fun k->k "download fails %s (dotdot)" path);
          S.Response.fail ~code:403 "Path is forbidden";
        ) else if not (VFS.contains path) then (
          S.log ~lvl:2 (fun k->k "download fails %s (not found)" path);
          S.Response.fail ~code:404 "File not found";
        ) else if S.Request.get_header req H.If_None_Match = Some (Lazy.force mtime) then (
          S.Response.make_raw ~code:304 ""
        ) else if VFS.is_directory path then (
          let parent = Filename.(dirname path) in
          let parent = if Filename.basename path <> "." then Some parent else None in
          match config.dir_behavior with
            | Index | Index_or_lists when VFS.contains (path // "index.html") ->
               (* redirect using path, not full path *)
               let new_path = "/" // prefix // path // "index.html" in
               S.log ~lvl:2 (fun k->k "download redirect %s" path);
               S.Response.make_raw ~code:301 "No Body"
                 ~headers:S.Headers.(empty |> set H.Location new_path
                                     |> set H.Content_Type "text/plain")
            | Lists | Index_or_lists ->
               let body = html_list_dir ~prefix vfs path ~parent
                          |> Html.to_string_top in
               S.log ~lvl:2 (fun k->k "download index %s" path);
               S.Response.make_string
                 ~headers:[header_html; H.ETag, Lazy.force mtime]
                 body
            | Forbidden | Index ->
               S.log ~lvl:2 (fun k->k "download index fails %s (forbidden)" path);
               S.Response.make_raw ~code:405 "listing dir not allowed"
        ) else (
          let mime =  Magic_mime.lookup path in
          let mime_type = (H.Content_Type,mime) in
          let size = VFS.file_size path in
          let accept_encoding =
            match S.Request.(get_header req H.Accept_Encoding)
            with None -> []
               | Some l -> List.map String.trim (String.split_on_char ',' l)
          in
          let cache = config.cache ~size ~mime ~accept_encoding in
          let fn path =
            try
              match cache with
              | MemCache ->
                 let string = VFS.read_file_content path in
                 S.log ~lvl:2 (fun k->k "download ok %s" path);
                 S.Response.make_raw
                   ~headers:(mime_type::[(H.ETag, Lazy.force mtime)])
                   ~code:200 string
              | CompressCache(encoding,cmp) ->
                 let string = cmp (VFS.read_file_content path) in
                 S.log ~lvl:2 (fun k->k "download ok %s" path);
                 S.Response.make_raw
                   ~headers:(mime_type::[(H.ETag, Lazy.force mtime)
                                       ;(H.Content_Encoding, encoding)])
                   ~code:200 string
              | SendFileCache ->
                 let (size, stream) = VFS.read_file_fd path in
                 (*if size > 50_000 then*)
                   begin
                     S.log ~lvl:2 (fun k->k "download ok %s" path);
                     S.Response.make_raw_file
                       ~headers:(mime_type::[(H.ETag, Lazy.force mtime)])
                       ~code:200 ~close:false size stream
                   end
              | NoCache | SendFile -> assert false
            with e ->
              S.log ~lvl:2 (fun k->k "download fails %s (%s)" path (D.printexn e));
              S.Response.fail ~code:500 "error while reading file: %s" (D.printexn e)
          in
          match cache with
          | NoCache ->
             let stream = VFS.read_file_stream path in
             S.log ~lvl:2 (fun k->k "download ok %s" path);
             S.Response.make_raw_stream
               ~headers:(mime_type::[(H.ETag, Lazy.force mtime)])
               ~code:200 stream
          | SendFile ->
             let (size, stream) = VFS.read_file_fd path in
             (*if size > 50_000 then*)
             begin
               S.log ~lvl:2 (fun k->k "download ok %s" path);
               S.Response.make_raw_file
                 ~headers:(mime_type::[(H.ETag, Lazy.force mtime)])
                 ~code:200 ~close:true size stream
             end
          | _ -> search_cache fn (cache_key cache,path)
        )
      )
  ) else (
    S.add_route_handler server ~filter ~meth:GET (route())
      (fun _ _  -> S.Response.make_raw ~code:405 "download not allowed");
  );
  ()

let add_vfs ?filter ~config ~vfs ~prefix server : unit =
  add_vfs_ ?filter ~config ~prefix ~vfs server

let add_dir_path ?filter ~config ~dir ~prefix server : unit =
  add_vfs_ ?filter ~config ~prefix ~vfs:(vfs_of_dir dir) server

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
        | Some (File {content;_}) -> Input.of_string content
        | _ -> failwith (Printf.sprintf "no such file: %S" p)

      let read_file_fd _ = failwith "read_file_fd not available"

      let list_dir p = match find_ self p with
        | Some (Dir sub) ->
          Str_map.fold (fun sub _ acc -> sub::acc) sub.entries [] |> Array.of_list
        | _ -> failwith (Printf.sprintf "no such directory: %S" p)

      let create _ = failwith "Embedded_fs is read-only"
      let delete _ = failwith "Embedded_fs is read-only"

    end in (module M)

end
