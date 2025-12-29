type t

type data
type 'a key

val new_key : ?cleanup_delete:('a -> unit) ->
              ?cleanup_no_client:('a -> bool) ->
              ?save:(out_channel -> 'a -> unit) ->
              ?load:(in_channel -> 'a) ->
              string -> 'a key

(** Managment of sessions using cookies *)

type cookie_policy =
  { path : string
  ; base : string
  ; life : float
  ; filter : Cookies.cookie -> Cookies.cookie option }

val default_cookie_policy : cookie_policy

val start_check: ?create:bool ->
            ?check:(t -> bool) ->
            ?cookie_policy:cookie_policy ->
            ?nosession:exn ->
            ?error:(Response_code.t*Headers.t) ->
            'a Request.t -> Cookies.t * t

val filter : ?check:(t -> bool) ->
            ?cookie_policy:cookie_policy ->
            ?error:(Response_code.t*Headers.t) ->
            'a Route.Filter.t

val get_session : ?cookie_policy:cookie_policy -> 'a Request.t -> t option

val get_session_data : t -> 'a key -> 'a option
val set_session_data : t -> 'a key -> 'a -> unit
val remove_session_data : t -> 'a key -> unit

exception Bad_session_cookie

val check_session_cookie : ?cookie_policy:cookie_policy -> ?create:bool -> 'a Request.t ->
                           t option

(** remove all server side and client side session data by expiring the
    session cookies*)
val delete_session : ?cookie_policy:cookie_policy -> 'a Request.t -> Cookies.t

val mk_cookies : t -> cookie_policy -> Cookies.t -> Cookies.t
val select_cookies : ?delete:bool -> ?create:t -> cookie_policy -> Cookies.t -> Cookies.t

val save_name     : string
val save_sessions : out_channel -> unit
val load_sessions : string * int -> in_channel -> unit

val get_session_key : t -> string
