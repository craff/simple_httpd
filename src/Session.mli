type session = Async.session
type session_data = Async.session_data

(** Managment of sessions using cookies *)

val check : ?session_life_time:float ->
            ?init:(unit -> session_data) ->
            ?finalise:(session_data -> unit) ->
            ?check:(session -> bool) ->
            ?error:(int*Headers.t) ->
            'a Route.filter

val get_session : 'a Request.t -> session

exception NoSession

val get_session_data : session -> session_data

val set_session_data : session -> session_data -> unit

val do_session_data :
  session -> (session_data -> 'a * session_data) -> 'a

val get_session_cookie : session -> string -> string option

val set_session_cookie : session -> string -> string -> unit

(** remove all server side session data *)
val delete_session : session -> unit
