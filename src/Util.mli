(** {1 Some utils for writing web servers} *)

val maxfd : int
val single_write : Unix.file_descr -> Bytes.t -> int -> int -> int
val read : Unix.file_descr -> Bytes.t -> int -> int -> int
val sendfile : Unix.file_descr -> Unix.file_descr -> int -> int -> int
val ssl_sendfile : Ssl.socket -> Unix.file_descr -> int -> int -> int

val setsockopt_cork : Unix.file_descr -> bool -> unit

val get_socket_error : ?default:Unix.error -> Unix.file_descr -> Unix.error option

val file_descr_to_int : Unix.file_descr -> int

val file_descr_of_int : int -> Unix.file_descr

val percent_encode : ?skip:(char -> bool) -> string -> string
(** Encode the string into a valid path following
    {{:https://tools.ietf.org/html/rfc3986#section-2.1}rfc 3986 section 2.1}.
    @param skip if provided, allows to preserve some characters, e.g. '/' in a path.
*)

val percent_decode : string -> string
(** Inverse operation of {!percent_encode}.
    Can raise [Invalid_argument "percent_decode"] if string is not valid
    percent encodings. *)

val split_query : string -> string * string
(** Split a path between the path and the query *)

val split_on_slash : string -> string list
(** Split a string on ['/'], remove the trailing ['/'] if any. *)

val get_non_query_path : string -> string
(** get the part of the path that is not the query parameters. *)

val get_query : string -> string
(** Obtain the query part of a path. *)

exception Invalid_query of string
val parse_query : string -> (string*string) list
(** Parse a query as a list of ['&'] or [';'] separated [key=value] pairs.
    The order might not be preserved. 0.3 *)

val pp_date : Format.formatter -> Unix.tm -> unit
(** Print date (given in GMT) in the expected format for http (for instance
    for expiration date of cookies. *)

val date_of_epoch : float -> string
(** Convert epoch to http date *)

val date_to_epoch : string -> float
(** Convert date to epoch *)

module LinkedList : sig
  type 'a t
  type 'a cell

  val create : unit -> 'a t
  val new_cell : 'a -> 'a cell
  val fake_cell : 'a cell
  val is_empty : 'a t -> bool
  val size : 'a t -> int
  val add_first : 'a -> 'a t -> 'a cell
  val add_last : 'a -> 'a t -> 'a cell
  val search_and_remove : ('a -> bool) -> ('a -> unit) -> 'a t -> unit

  val is_cell : 'a cell -> bool
  val get : 'a cell -> 'a
  val head : 'a t -> 'a cell
  val tail : 'a t -> 'a cell
  val next : 'a cell -> 'a cell
  val prev : 'a cell -> 'a cell
  val remove_cell : 'a cell -> 'a t -> unit
  val move_first : 'a cell -> 'a t -> unit
  val move_last : 'a cell -> 'a t -> unit
end

val update_atomic : 'a Atomic.t -> ('a -> 'a) -> unit
val get_update_atomic : 'a Atomic.t -> ('a -> 'b * 'a) -> 'b

val addr_of_sock : Unix.file_descr -> string

val to_human : ?unit:string -> float -> string
val to_human_int : ?unit:string -> int -> string

type 'a key
type data

val new_key : 'a. ('a -> unit) -> 'a key
val search : 'a key -> data -> 'a
val add_replace : 'a key -> 'a -> data -> data
val remove : 'a key -> data -> data
val cleanup : data -> unit
val empty : data
