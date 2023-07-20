
val html : ?log_size:int -> Server.t ->
           'a Request.t -> Headers.t -> Headers.t * Cookies.t * Input.t
(** Returns a detailed server status as html *)
