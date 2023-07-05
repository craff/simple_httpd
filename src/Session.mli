type t
type session_data = Async.session_data

(** Managment of sessions using cookies *)

val check: ?session_life_time:float ->
            ?init:(unit -> session_data) ->
            ?finalise:(session_data -> unit) ->
            ?check:(t -> bool) ->
            ?filter:(Http_cookie.t -> Http_cookie.t option) ->
            ?error:(Response_code.t*Headers.t) ->
            'a Request.t -> Cookies.t * t

val filter : ?session_life_time:float ->
            ?init:(unit -> session_data) ->
            ?finalise:(session_data -> unit) ->
            ?check:(t -> bool) ->
            ?filter:(Http_cookie.t -> Http_cookie.t option) ->
            ?error:(Response_code.t*Headers.t) ->
            'a Route.Filter.t

val get_session : 'a Request.t -> t

exception NoSession

val get_session_data : t -> session_data

val set_session_data : t -> session_data -> unit

val do_session_data :
  t -> (session_data -> 'a * session_data) -> 'a

(** remove all server side session data by expiring the session cookies*)
val delete_session : t -> unit
