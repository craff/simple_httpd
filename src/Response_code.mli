(** {1 Response Codes}

    Response code allows client to know if a request failed and give a reason.
    This module is not complete (yet). *)



type t = int
(** A standard HTTP code.

    https://tools.ietf.org/html/rfc7231#section-6 *)

val ok : t
(** The code [200] *)

val not_found : t
(** The code [404] *)

val descr : t -> string
(** A description of some of the error codes.
    NOTE: this is not complete (yet). *)

exception Bad_req of int * string
val bad_reqf : t -> ('a, unit, string, 'b) format4 -> 'a
