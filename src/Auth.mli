
module type Login = sig
  type t
  val check : login:string -> password:string -> t option (* None: bad login *)
  val login_url : string
end

module Make(Login:Login) : sig
  val login_page : Html.chaml

  val check : 'a Request.t -> (Cookies.t * Session.t)

  val logout_page : string -> 'a Request.t -> 'b
end
