type t

type data
type 'a key

val new_key : unit -> 'a key
val new_key_with_cleanup : ('a -> unit) -> 'a key

(** Managment of sessions using cookies *)

val check: ?session_life_time:float ->
            ?check:(t -> bool) ->
            ?filter:(Http_cookie.t -> Http_cookie.t option) ->
            ?error:(Response_code.t*Headers.t) ->
            'a Request.t -> Cookies.t * t

val filter : ?session_life_time:float ->
            ?check:(t -> bool) ->
            ?filter:(Http_cookie.t -> Http_cookie.t option) ->
            ?error:(Response_code.t*Headers.t) ->
            'a Route.Filter.t

val get_session : 'a Request.t -> t

exception NoSession

val get_session_data : t -> 'a key -> 'a
val set_session_data : t -> 'a key -> 'a -> unit
val remove_session_data : t -> 'a key -> unit

(** remove all server side session data by expiring the session cookies*)
val delete_session : t -> unit
