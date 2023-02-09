
val filter :
  ?compress_above:int ->
  ?buf_size:int -> unit ->
  Simple_httpd_server.filter
(** Middleware responsible for deflate compression/decompression.
    @since 0.11 *)

val deflate_string : ?buf_size:int -> string -> string
(** zlib string compression *)

val accept_deflate : 'a Simple_httpd_server.Request.t -> bool
(** check if request accept deplate *)
