
val html : ?log_size:int -> ?md5_pass:Digest.t -> Server.t ->
           'a Request.t -> Headers.t -> Headers.t * Cookies.t * Input.t
(** Returns a detailed server status as html *)
