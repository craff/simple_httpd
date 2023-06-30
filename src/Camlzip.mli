
val filter :
  ?compress_above:int ->
  ?buf_size:int -> unit -> Input.t Route.Filter.t
(** Middleware responsible for deflate compression/decompression.
    @since 0.11 *)

val deflate_string : ?buf_size:int -> string -> string
(** zlib string compression *)

val accept_deflate : 'a Request.t -> bool
(** check if request accept deplate *)

val file_deflate : string -> string -> unit
