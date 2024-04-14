(** {1 Cookies}

    Cookies are data that are maintend both on server and clients.
    This is a module to get and set cookies in the headers. *)

type t = Http_cookie.t list

val empty : t
val parse : string -> t
val add : Http_cookie.t -> t -> t
val create : ?path:string ->
             ?domain:string ->
             ?expires:Http_cookie.date_time ->
             ?max_age:int64 ->
             ?secure:bool ->
             ?http_only:bool ->
             ?same_site:Http_cookie.same_site ->
             ?extension:string ->
             name:string ->
             string -> t -> t

val get : string -> t -> Http_cookie.t
