(** {1 Methods}

    A short module defining the various HTTP methods (GET,PUT,...)*)

type t =
  | GET
  | PUT
  | POST
  | HEAD
  | DELETE
(** A HTTP method.
    For now we only handle a subset of these.

    See https://tools.ietf.org/html/rfc7231#section-4 *)

val pp : Format.formatter -> t -> unit
val to_string : t -> string
val of_string : string -> t
val parse : Input.t -> t
