(** {1 Headers}

    Headers are metadata associated with a request or response. This module provide
    the necessary function to read and modify headers *)

(** A module defining all the legal header names *)

(** @inline *)
include module type of Headers_

type t = (header * string) list
(** The header files of a request or response.

    Neither the key nor the value can contain ['\r'] or ['\n'].
    See https://tools.ietf.org/html/rfc7230#section-3.2 *)

val empty : t
(** Empty list of headers *)

val get : ?f:(string->string) -> header -> t -> string option
(** [get k headers] looks for the header field with key [k].
    @param f if provided, will transform the value before it is returned. *)

val get_exn : ?f:(string->string) -> header -> t -> string
(** [get_exn k headers] same as above but raise [Not_found] if the headers is
    not present. *)

val set : header -> string -> t -> t
(** [set k v headers] sets the key [k] to value [v].
    It erases any previous entry for [k] *)

val set_cookies : Cookies.t -> t -> t
(** Encode all the cookies in the header *)

val remove : header -> t -> t
(** Remove the key from the headers, if present. *)

val contains : header -> t -> bool
(** Is there a header with the given key? *)

val pp : Format.formatter -> t -> unit
(** Pretty print the headers. *)

val parse_ : buf:Buffer.t -> Input.t -> t * Cookies.t
(** For internal use *)
