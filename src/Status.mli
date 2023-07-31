
val html : ?log_size:int -> ?check:('a Request.t -> Cookies.t * Session.t)
           -> ?in_head : Html.elt -> ?in_body : Html.elt
           -> Server.t -> 'a Request.t -> Headers.t -> Headers.t * Cookies.t * Input.t
(** Returns a detailed server status as html *)
