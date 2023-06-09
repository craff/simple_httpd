type session = Async.session
type session_data = Async.session_data

(** Managment of sessions using cookies *)

val check : ?session_life_time:float ->
            ?init:(unit -> session_data) ->
            ?check:(session -> bool) ->
            ?error:(int*string) ->
            Route.filter

val do_session_data : (session_data -> 'a) -> session -> 'a

val set_session_data : session -> session_data -> (session_data -> unit) -> unit

val set_session_cookie : session -> string -> string -> unit

(** remove all server side session data *)
val delete_session : session -> unit
