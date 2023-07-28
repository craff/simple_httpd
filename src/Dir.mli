
(** Serving static content from directories

    This module provides the same functionality as the "http_of_dir" tool.
    It exposes a directory (and its subdirectories), with the optional ability
    to delete or upload files. *)

(** behavior of static directory.

    This controls what happens when the user requests the path to
    a directory rather than a file. *)
type dir_behavior =
  | Index
  (** Redirect to index.html if present, else fails. *)
  | Lists
  (** Lists content of directory. Be careful of security implications. *)
  | Index_or_lists
  (** Redirect to index.html if present and lists content otherwise.
      This is useful for tilde ("~") directories and other per-user behavior,
      but be mindful of security implications *)
  | Forbidden
  (** Forbid access to directory. This is suited for serving assets, for example. *)

(** configuration for static file handlers. This might get
    more fields over time. *)
type config = {
  mutable download: bool;
  (** Is downloading files allowed? *)

  mutable dir_behavior: dir_behavior;
  (** Behavior when serving a directory and not a file *)

  mutable delete: bool;
  (** Is deleting a file allowed? (with method DELETE) *)

  mutable upload: bool;
  (** Is uploading a file allowed? (with method PUT) *)

  mutable max_upload_size: int;
  (** If {!upload} is true, this is the maximum size in bytes for
      uploaded files. *)
}

(** default configuration: [
  { download=true
  ; dir_behavior=Forbidden
  ; delete=false
  ; upload=false
  ; max_upload_size = 10 * 1024 * 1024
  }] *)
val default_config : unit -> config

val config :
  ?download:bool ->
  ?dir_behavior:dir_behavior ->
  ?delete:bool ->
  ?upload:bool ->
  ?max_upload_size:int ->
  unit ->
  config
(** Build a config from {!default_config}. *)

(** [add_dirpath ~config ~dir ~prefix server] adds route handle to the
    [server] to serve static files in [dir] when url starts with [prefix],
    using the given configuration [config]. *)
val add_dir_path :
  ?addresses: Address.t list ->
  ?filter:Input.t Route.Filter.t ->
  ?prefix:string ->
  ?config:config ->
  dir:string ->
  Server.t -> unit

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
  ; headers : Headers.t
  } -> file_info

(** Virtual file system.

    This is used to emulate a file system from pure OCaml functions and data,
    e.g. for resources bundled inside the web server.

    Remark: the diffrence between VFS and cache is that caches are updated
    when the modification time of the file changes. Thus, VFS do not do any
    system call.
 *)
module type VFS = sig
  val descr : string
  (** Description of the VFS *)

  val is_directory : string -> bool

  val contains : string -> bool
  (** [file_exists vfs path] returns [true] if [path] points to a file
      or directory inside [vfs]. *)

  val list_dir : string -> string array
  (** List directory. This only returns basenames, the files need
      to be put in the directory path using [Filename.concat]. *)

  val delete : string -> unit
  (** Delete path *)

  val create : string -> (bytes -> int -> int -> unit) * (unit -> unit)
  (** Create a file and obtain a pair [write, close] *)

  val read_file : string -> file_info
  (** Read content of a file *)
end

val vfs_of_dir : string -> (module VFS)
(** [vfs_of_dir dir] makes a virtual file system that reads from the
    disk.
*)

val add_vfs :
  ?addresses: Address.t list ->
  ?filter:Input.t Route.Filter.t ->
  ?prefix:string ->
  ?config:config ->
  vfs:(module VFS) ->
  Server.t -> unit
(** Similar to {!add_dir_path} but using a virtual file system instead.
*)

(** An embedded file system, as a list of files with (relative) paths.
    This is useful in combination with the "simple-httpd-mkfs" tool,
    which embeds the files it's given into a OCaml module.
*)
module Embedded_fs : sig
  type t
  (** The pseudo-filesystem *)

  val create : ?top:string -> ?mtime:float -> unit -> t

  val add_file : t -> path:string -> ?mtime:float -> ?headers:Headers.t ->
                 string -> unit
  (** Add file to the virtual file system.
      @raise Invalid_argument if the path contains '..' or if it tries to
      make a directory out of an existing path that is a file. *)

  val add_dynamic : t -> path:string -> ?mtime: float -> ?headers:Headers.t ->
                    dynamic -> unit

  val add_path : t -> path:string -> ?mtime:float -> ?headers:Headers.t ->
                 ?deflate:string -> string -> unit

  val to_vfs : t -> (module VFS)
end
