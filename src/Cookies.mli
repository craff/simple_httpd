(** {1 Cookies}

    Cookies are data that are maintend both on server and clients.
    This is a module to get and set cookies in the headers. *)

type same_site = None | Lax | Strict
type cookie = {
    name : string;
    value : string;
    path : string option;
    domain : string option;
    expires : Unix.tm option;
    max_age : int64 option;
    secure : bool;
    http_only : bool;
    same_site : same_site;
    extension : string option;
  }
type t = cookie list

val empty : t
val add : cookie -> t -> t
val create : ?path:string ->
             ?domain:string ->
             ?expires:Unix.tm ->
             ?max_age:int64 ->
             ?secure:bool ->
             ?http_only:bool ->
             ?same_site:same_site ->
             ?extension:string ->
             name:string ->
             string -> t -> t

val get : string -> t -> cookie

exception Invalid_cookie of string

val parse : string -> t

val cookie_to_string : cookie -> string

val expire : cookie -> cookie

val value : cookie -> string
val name : cookie -> string
