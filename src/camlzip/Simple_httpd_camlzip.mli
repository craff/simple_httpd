
val middleware :
  ?compress_above:int ->
  ?buf_size:int -> unit ->
  Simple_httpd_server.Middleware.t
(** Middleware responsible for deflate compression/decompression.
    @since 0.11 *)

val setup :
  ?compress_above:int ->
  ?buf_size:int -> Simple_httpd_server.t -> unit
(** Install middleware for simple_httpd to be able to encode/decode
    compressed streams
    @param compress_above threshold above with string responses are compressed
    @param buf_size size of the underlying buffer for compression/decompression *)

val deflate_string : ?buf_size:int -> string -> string
(** zlib string compression *)

val accept_deflate : 'a Simple_httpd_server.Request.t -> bool
(** check if request accept deplate *)
