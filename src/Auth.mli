
module type Login = sig
  type t
  val check : login:string -> password:string -> t option (* None: bad login *)
  val login_url : string
  val cookie_policy : Session.cookie_policy
end

module Make(Login:Login) : sig
  val login_page : ?destination:string -> ?page:Html.chaml -> Html.chaml

  val logout_page : ?destination:string -> ?page:Html.chaml -> Html.chaml

  val check : ?nologin:(Response_code.t * Headers.t * string) ->
              ?check_data:(Login.t -> bool)
              -> 'a Request.t -> (Cookies.t * Session.t)

  val check_handler : ?not_logged:('a Request.t -> Response.t)
                      -> ('a Request.t -> Cookies.t -> Login.t -> Response.t)
                      -> ('a Request.t -> Response.t)

  val check_filter :
    ?nologin:(Response_code.t * Headers.t * string) ->
    ?check_data:(Login.t -> bool)
    -> 'a Route.Filter.t

  val get_session_data : 'a Request.t -> Login.t option

end
