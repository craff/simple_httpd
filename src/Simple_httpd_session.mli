open Simple_httpd_domain

(** Managment of sessions using cookies *)

val check : ?session_life_time:float ->
            ?init:(unit -> session_data) ->
            ?check:(session -> bool) ->
            ?error:(int*string) ->
            'a Simple_httpd_server.Request.t ->
              Simple_httpd_server.finaliser

val do_session_data : (session_data -> 'a) -> session -> 'a

val set_session_data : session -> session_data -> (session_data -> unit) -> unit

val set_session_cookie : session -> string -> string -> unit

(** remove all server side session data *)
val delete_session : session -> unit
