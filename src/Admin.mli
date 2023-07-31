
module type Auth = sig
  val password : Digest.t
  val login_url : string
end

module Make(Auth:Auth) : sig
  val login_page : Html.chaml

  val check : 'a Request.t -> (Cookies.t * Session.t)

  val logout_page : string -> 'a Request.t -> 'b
end
