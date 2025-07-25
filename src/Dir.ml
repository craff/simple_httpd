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

let contains_dotdot ls =
  try
    List.iter (fun s -> if s = ".." then raise Exit) ls;
    false
  with Exit -> true

(* Human readable size *)
let human_size (x:int) : string =
  if x >= 1_000_000_000 then Printf.sprintf "%d.%dG" (x / 1_000_000_000) ((x/1_000_000) mod 1_000_000)
  else if x >= 1_000_000 then Printf.sprintf "%d.%dM" (x / 1_000_000) ((x/1000) mod 1_000)
  else if x >= 1_000 then Printf.sprintf "%d.%dk" (x/1000) ((x/100) mod 100)
  else Printf.sprintf "%db" x

let header_html = [Headers.Content_Type, "text/html"]

let encode_path s = Util.percent_encode ~skip:(function '/' -> true|_->false) s

let is_hidden s = String.length s>0 && s.[0] = '.'

type dynamic = Html.chaml

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
  type path
  val path : string list -> path
  val concat : path -> string -> path
  val to_string : ?prefix:string -> path -> string
  val descr : string
  val is_directory : path -> bool
  val contains : path -> bool
  val list_dir : path -> string array
  val delete : path -> unit
  val create : path -> (bytes -> int -> int -> unit) * (unit -> unit)
  val read_file : path -> file_info
  val keep : path -> unit
  val free : path -> unit
end

type vfs = (module VFS)

let vfs_of_dir (top:string) : vfs =
  let module M = struct
    type path = string * string list * Util.file_type
    let path l =
      if contains_dotdot l then
        Response.fail_raise ~code:forbidden "Path is forbidden";
      let name = Util.fast_concat '/' (top :: l) in
      let ft = Util.file_type name in
      (name, l, ft)
    let concat (_ , l, _) f = path (l @ [f])
    let to_string ?(prefix="") (_, l, _) =
      let res = Util.fast_concat '/' (prefix :: l) in
      if String.length res = 0 || res.[0] != '/' then "/" ^ res else res
    let descr = top
    let is_directory =
      function (_,_,Util.Dir _) -> true
             | _ -> false
    let contains (_,_,ft) = (ft <> Util.Inexistant)
    let list_dir =
      function (_,_,Util.Dir { fd; _ }) ->
                begin
                  let r = ref [] in
                  try
                    while true do
                      let path = Unix.readdir fd in
                      if String.length path > 0 && path <> ".."
                         && path <> "." then
                        r := path :: !r
                    done;
                    assert false
                  with End_of_file -> Array.of_list !r
                end
             | _ -> [||]
    let create (f, _, _) =
      let oc = open_out_bin f in
      let write = output oc in
      let close() = close_out oc in
      write, close
    let delete (f, _, _) = Sys.remove f
    let cache = Hashtbl.create 1024
    let read_file = function
      | (f, _, Util.Reg({ fd; mtime; size; _})) ->
         let mtime = Some mtime in
         let content = Fd(fd) in
         let mtime2, size2, headers =
           try Hashtbl.find cache f
           with Not_found -> None, None, Headers.empty
         in
         let size, headers =
           if mtime2 <> mtime then
             begin
               let mime =  Magic_mime.lookup f in
               let headers = [Headers.Content_Type, mime] in
               Hashtbl.replace cache f (mtime, Some size, headers);
               Some size, headers
             end
           else
             size2,headers
         in
         FI { content; size; mtime; headers }
      | (f, _, Util.Dir({ fd; mtime; _})) ->
         let mtime = Some mtime in
         let content = Dir(fd) in
         let mtime2, size2, headers =
           try Hashtbl.find cache f
           with Not_found -> None, None, Headers.empty
         in
         let size, headers =
           if mtime2 <> mtime then
             begin
               let mime =  Magic_mime.lookup f in
               let headers = [Headers.Content_Type, mime] in
               Hashtbl.replace cache f (mtime, None, headers);
               None, headers
             end
           else
             size2,headers
         in
         FI { content; size; mtime; headers }
      | _ -> raise Not_found
    let keep (_, _, f) = match f with
      | Util.Reg r -> r.free <-false
      | Util.Dir r -> r.free <-false
      | _ -> ()
    let free (_, _, f) = Util.free f
  end in
  (module M)


(* @param on_fs: if true, we assume the file exists on the FS *)
let add_vfs_ ?addresses ?(filter=(fun x -> (x, fun r -> r)))
               ?(config=default_config ())
               ?(prefix="") ~vfs:(module VFS:VFS) server : unit=
  let html_list_dir ~prefix ~parent d : Html.chaml =
    let entries = VFS.(list_dir d) in
    Array.sort String.compare entries;

    (* TODO: breadcrumbs for the path, each element a link to the given ancestor dir *)
    let head =
      {html|
      <head>
	<title>list directory "{`prefix`}"</title>
	<meta charset="utf-8"/>
      </head>
      |html}
    in
    let n_hidden = ref 0 in
    Array.iter (fun f -> if is_hidden f then incr n_hidden) entries;

    let file_to_elt (f : string) : string =
      let fpath = VFS.concat d f in
      if not @@ VFS.contains fpath then (
        {html|<li>{`f`} [invalid file]</li>|html}
      ) else (
        let is_dir = VFS.is_directory fpath in
        let size = if is_dir then "" else
          try
            match VFS.read_file fpath with
            | FI { size = Some f ; _ } ->
               Printf.sprintf " (%s)" @@ human_size f
            | _ -> ""
          with Not_found -> ""
        in
        let tpath = VFS.to_string ~prefix fpath in
        VFS.free fpath;
        {html|
        <li>
	    <a href={`encode_path tpath`}>
		{`f`} </a>
		{`if is_dir then " dir" else size`}

        </li>
        |html}
      )
    in
    {chaml|
    <!DOCTYPE html>
    <html>
      {`head`}
      <body>
        <h2>Index of "{`VFS.to_string ~prefix d`}"</h2>
        <ml>
          begin
            match parent with
          | None -> ()
          | Some p -> echo {html|
              <a href={`encode_path p`}>parent directory</a>
              |html}
          end;;
         </ml>
         <ul>
         <ml>
           if !n_hidden>0 then
             {funml|
             <details>({` string_of_int !n_hidden`} hidden files)
                 <ml>Array.iter (fun f -> if is_hidden f then echo (file_to_elt f))
                          entries</ml>
               </details>|funml} output;
             Array.iter (fun f -> if not (is_hidden f) then echo (file_to_elt f))
               entries
           </ml>
         </ul>
       </body>
     </html>
     |chaml}
  in
  let route () =
    if prefix="" then Route.rest
    else let prefix = List.rev (Util.split_on_slash prefix) in
         List.fold_left (fun acc s -> Route.exact_path s acc) Route.rest prefix
  in
  let do_path ~must_exists fn lpath =
    let path = VFS.path lpath in
    try
      if must_exists && not (VFS.contains path) then (
        Route.pass ());
      (fun req ->
        let r = fn path req in
        VFS.free path;
        r)
    with e ->
      VFS.free path; raise e
  in
  if config.delete then (
    Server.add_route_handler ?addresses ~filter ~meth:DELETE
      server (route())
      (do_path ~must_exists:true (fun path _req ->
           Response.make_string
             (try
                log (Req 1) (fun k->k "done delete %s"  (VFS.to_string path));
                VFS.delete path; "file deleted successfully"
              with e ->
                log (Exc 0) (fun k->k "delete fails %s (%s)"  (VFS.to_string path)
                                         (Async.printexn e));
                Response.fail_raise ~code:internal_server_error
                  "delete fails: %s (%s)"  (VFS.to_string path) (Async.printexn e))
      )))
    else (
      Server.add_route_handler ?addresses ~filter ~meth:DELETE server (route())
        (fun _ _  ->
          Response.fail_raise ~code:method_not_allowed "delete not allowed");
    );

  if config.upload then
    Server.add_route_handler_stream ?addresses ~meth:PUT server (route())
      ~filter:(fun req ->
          match Request.get_header_int req Headers.Content_Length with
          | Some n when n > config.max_upload_size ->
             Response.fail_raise ~code:forbidden
               "max upload size is %d" config.max_upload_size
          | _ -> filter req
        )
      (do_path ~must_exists:false (fun path req ->
         let write, close =
           try VFS.create path
           with e ->
             log (Exc 0) (fun k->k "fail uploading %s (%s)"
                                       (VFS.to_string path) (Async.printexn e));
             Response.fail_raise ~code:forbidden "cannot upload to %S: %s"
                (VFS.to_string path) (Async.printexn e)
         in
         let req = Request.limit_body_size ~max_size:config.max_upload_size req in
         Input.iter write req.Request.body;
         close ();
         log (Req 1) (fun k->k "done uploading %s"  (VFS.to_string path));
         Response.make_raw ~code:created "upload successful"
      ))
    else (
    Server.add_route_handler ?addresses ~filter ~meth:PUT server (route())
      (fun _ _  -> Response.make_raw ~code:method_not_allowed
                     "upload not allowed");
  );

  if config.download then
    Server.add_route_handler ?addresses ~filter ~meth:GET server (route())
      (do_path ~must_exists:true (fun path req ->
        if VFS.is_directory path then
          begin
            match config.dir_behavior, VFS.concat path "index.html" with
            | (Index | Index_or_lists), index_path when VFS.contains index_path ->
               let host = match Request.get_header req Headers.Host with
                 | Some h -> h
                 | None -> raise Not_found
               in
               let url =
                 Printf.sprintf "https://%s%s" host
                   (VFS.to_string ~prefix index_path)
               in
               Log.f (Exc 0) (fun k -> k "redirect %s %s => %s"
                                         prefix (VFS.to_string path) url);
               let headers = [Headers.Location, url] in
               VFS.free index_path;
               Response.fail_raise ~headers
                 ~code:Response_code.permanent_redirect
                 "Temporary redirect"
            | _, index_path ->
               VFS.free index_path;
            | exception e ->
               Log.f (Exc 0) (fun k -> k "exception in search index %s"
                                         (Printexc.to_string e))
          end;
        let FI info = try VFS.read_file path with Not_found -> assert false
        in
        let mtime, may_cache =
           match info.mtime with
           | None -> None, false
           | Some t ->
              let mtime_str = Printf.sprintf "\"%.4f\"" t in
              let may_cache =
                match Request.get_header req Headers.If_None_Match with
                | Some mtime -> mtime = mtime_str
                | None ->
                match Request.get_header req Headers.If_Modified_Since with
                | Some str ->
                   (try Util.date_to_epoch str <= t with
                      _ -> false)
                | None -> false
              in
         (Some (t, mtime_str), may_cache)
        in
        if may_cache then Response.make_raw ~code:not_modified "" else
        let cache_control h =
          match mtime with
          | None -> (Headers.Cache_Control, "no-store") :: h
          | Some (mtime, mtime_str) ->
             Headers.((ETag, mtime_str)
                      :: (Age, (string_of_float (Request.start_time req -. mtime)))
                      :: (Cache_Control, "public,no-cache")
                      :: h)
        in
        if VFS.is_directory path then (
          let parent = Some (Filename.(dirname (VFS.to_string ~prefix path))) in
          match config.dir_behavior with
            | Lists | Index_or_lists ->
               let body = html_list_dir ~prefix path ~parent in
               log (Req 1) (fun k->k "download index %s" (VFS.to_string path));
               let (headers, _cookies, str) = body req header_html in
               Response.make_stream ~headers str
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
               ~headers:(cache_control (
                         (Headers.Content_Encoding, "deflate"):: info.headers))
               ~code:ok size (Util.Sfd.make fd)
          | Path(f, _) ->
             let fd = Unix.openfile f [O_RDONLY] 0 in
             let size = match info.size with Some s -> s | None -> assert false in
             Response.make_raw_file
               ~headers:(cache_control info.headers)
               ~code:ok size (Util.Sfd.make fd)
          | Fd(fd) ->
             VFS.keep path;
             let size = Unix.(fstat fd).st_size in
             Response.make_raw_file
               ~headers:(cache_control info.headers)
               ~code:ok size (Util.Sfd.make fd)
          | String(_, Some sz) when deflate ->
             Response.make_raw
               ~headers:(cache_control
                         Headers.((Content_Encoding, "deflate") :: info.headers))
               ~code:ok sz
          | String(s, _) ->
             Response.make_raw
               ~headers:(cache_control info.headers)
               ~code:ok s
          | Dynamic f ->
             let headers = cache_control info.headers in
             let headers, _cookies, input = f req headers in
             Response.make_raw_stream ~headers ~code:ok input
          | Stream input ->
             Response.make_raw_stream
               ~headers:(cache_control info.headers)
               ~code:ok input

          | Dir _ -> assert false

        )
     )
  ) else (
    Server.add_route_handler ?addresses ~filter ~meth:GET server (route())
      (fun _ _  -> Response.make_raw ~code:method_not_allowed "download not allowed");
  );
  ()

let add_vfs ?addresses ?filter ?prefix ?config ~vfs server : unit =
  add_vfs_ ?addresses ?filter ?prefix ?config ~vfs server

let add_dir_path ?addresses ?filter ?prefix ?config ~dir server : unit =
  add_vfs_ ?addresses ?filter ?prefix ?config ~vfs:(vfs_of_dir dir) server

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

  let add_file_gen (self:t) ~path content : unit =
    if List.mem ".." path then (
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
              (Printf.sprintf "in path %S, %S is a file, not a directory"
                 (Util.fast_concat '/' path) d)
          | exception Not_found ->
             let sub = create ~mtime:self.emtime () in
             let entry =
               { kind = Dir sub; mtime = Some self.emtime;
                 size = None; headers = Headers.empty }
             in
             Hashtbl.add self.entries d entry;
             sub
        in
        loop sub ds
    in
    loop self path

  let add_file (self:t) ~path ?mtime ?(headers=Headers.empty) content : unit =
    let path = Util.split_on_slash path in
    if contains_dotdot path then invalid_arg "file contains ..";
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

  let add_dynamic (self:t) ~path ?mtime ?(headers=Headers.empty) content : unit =
    let path = Util.split_on_slash path in
    if contains_dotdot path then invalid_arg "file contains ..";
    let entry = { mtime; headers; size = None; kind = Dynamic content} in
    add_file_gen (self:t) ~path entry

  let add_path (self:t) ~path ?mtime ?(headers=Headers.empty) ?deflate rpath : unit =
    (*let fz = rpath ^".zlib" in *)
    let path = Util.split_on_slash path in
    if contains_dotdot path then invalid_arg "file contains ..";
    let deflate = Option.map (fun x ->
                      let size = (Unix.stat x).st_size in
                      x, size) deflate in
    let content = Path(rpath, deflate) in
    let size = Some (Unix.stat rpath).st_size in
    let entry = { mtime; headers; size; kind = content} in
    add_file_gen (self:t) ~path entry

  (* find entry *)
  let find_ self (path:string list) : entry option =
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
    if path=[] then Some { mtime = Some self.emtime;
                           size = None;
                           kind = Dir self;
                           headers = Headers.empty }
    else loop self path

  let to_vfs self : vfs =
    let module M = struct
      type path = string list
      let path l = l
      let concat l x = l @ [x]
      let to_string ?(prefix="") l =
        let res = Util.fast_concat '/' (prefix :: l) in
        if String.length res = 0 || res.[0] != '/' then "/" ^ res else res

      let descr = "Embedded_fs"

      let read_file p =
        match find_ self p with
        | Some { mtime; headers; kind = content; size } ->
           FI { content; mtime; size; headers }
        | _ -> Response.fail_raise ~code:not_found "File %s not found"
                 (Util.fast_concat '/' p)

      let contains p = match find_ self p with
        | Some _ -> true
        | None -> false

      let is_directory p = match find_ self p with
        | Some { kind = Dir _; _ } -> true
        | _ -> false

      let list_dir p = match find_ self p with
        | Some { kind = Dir sub; _ } ->
          Hashtbl.fold (fun sub _ acc -> sub::acc) sub.entries [] |> Array.of_list
        | _ -> failwith (Printf.sprintf "no such directory: %S" (Util.fast_concat '/' p))

      let create _ = failwith "Embedded_fs is read-only"
      let delete _ = failwith "Embedded_fs is read-only"
      let keep   _ = ()
      let free   _ = ()
    end in (module M)
end
