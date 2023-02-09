type t

val create : ?buf_size:int -> Simple_httpd_domain.client -> t
val flush : t -> unit
val close : t -> unit
val add_char : t -> char -> unit
val add_decimal : t -> int -> unit
val add_hexa : t -> int -> unit
val add_string : t -> string -> unit
val add_substring : t -> string -> int -> int -> unit
val add_bytes : t -> bytes -> unit
val add_subbytes : t -> bytes -> int -> int -> unit
val printf : t -> ('a, unit, string, unit) format4 -> 'a

val output_chunked : t -> Simple_httpd_input.t -> unit
(** Write the stream into the channel, using the chunked encoding. *)

val output_str   : t -> string -> unit
val output_bytes : t -> bytes  -> unit
(** Write the content of a string/bytes to a fd *)
