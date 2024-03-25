type t

type data
type 'a key

val new_key : unit -> 'a key
val new_key_with_cleanup : ('a -> unit) -> 'a key

(** Managment of sessions using cookies *)

type cookie_policy =
  { path : string
  ; base : string
  ; life : float
  ; filter : Http_cookie.t -> Http_cookie.t option }

val default_cookie_policy : cookie_policy

val start_check: ?create:bool ->
            ?check:(t -> bool) ->
            ?cookie_policy:cookie_policy ->
            ?error:(Response_code.t*Headers.t) ->
            'a Request.t -> Cookies.t * t

val filter : ?check:(t -> bool) ->
            ?cookie_policy:cookie_policy ->
            ?error:(Response_code.t*Headers.t) ->
            'a Route.Filter.t

val get_session : ?cookie_policy:cookie_policy -> 'a Request.t -> t option

val get_session_data : t -> 'a key -> 'a
val set_session_data : t -> 'a key -> 'a -> unit
val remove_session_data : t -> 'a key -> unit

(** remove all server side and client side session data by expiring the
    session cookies*)
val delete_session : ?cookie_policy:cookie_policy -> 'a Request.t -> Cookies.t

val mk_cookies : t -> cookie_policy -> Cookies.t -> Cookies.t
