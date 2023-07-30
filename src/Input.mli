(** Input byte streams. *)

type hidden
(** Type used to make {!t} unbuildable via a record literal, but still gives
    access to its mutable field (for camlzip only). Use {!make} instead. *)

type t = {
  mutable bs: bytes;
  (** The bytes *)

  mutable off : int;
  (** Beginning of valid slice in {!bs} *)

  mutable len : int;
  (** Length of valid slice in {!bs}. If [len = 0] after
      a call to {!fill_buf}, then the stream is finished. *)

  fill_buf: unit -> unit;
  (** See the current slice of the internal buffer as [bytes, i, len],
      where the slice is [bytes[i] .. [bytes[i+len-1]]].
      Can block to refill the buffer if there is currently no content.
      If [len=0] then there is no more data. *)

  consume: int -> unit;
  (** Consume [n] bytes from the buffer.
      This should only be called with [n <= len]. *)

  close: unit -> unit;
  (** Close the stream. *)

  _rest: hidden;
  (** Use {!make} to build a stream. *)
}
(** A buffered stream, with a view into the current buffer (or refill if empty),
    and a function to consume [n] bytes. *)

val close : t -> unit
(** Close stream *)

val empty : t
(** Stream with 0 bytes inside *)

val of_chan : ?buf_size:int -> in_channel -> t
(** Make a buffered stream from the given channel. *)

val of_fd : ?buf_size:int -> Unix.file_descr -> t
(** Make a buffered stream from the given file descriptor. *)

val of_client : ?buf_size:int -> Async.client -> t
(** Make a buffered stream from the given http client. *)

val of_io : ?buf_size:int -> Async.Io.t -> t
(** Allow a to Make a buffered stream from the given {!Io.t}.
    The call will be scheduled if read blocks. *)

val of_bytes : ?i:int -> ?len:int -> bytes -> t
(** A stream that just returns the slice of bytes starting from [i]
    and of length [len]. *)

val of_string : string -> t

module type Output = sig
  val echo : string -> unit
  val printf : ('a, Format.formatter, unit, unit) format4 -> 'a
end

val of_output : ((module Output) -> unit) -> t

val iter : (bytes -> int -> int -> unit) -> t -> unit
(** Iterate on the chunks of the stream. *)

val to_chan : out_channel -> t -> unit
(** Write the stream to the channel. *)

val make :
  ?bs:bytes ->
  ?close:(t -> unit) ->
  consume:(t -> int -> unit) ->
  fill:(t -> unit) ->
  unit -> t
(** [make ~fill ()] creates a byte stream.
    @param fill is used to refill the buffer, and is called initially.
    @param close optional closing.
    @param init_size size of the buffer.
*)

val with_file : ?buf_size:int -> string -> (t -> 'a) -> 'a
(** Open a file with given name, and obtain an input stream
    on its content. When the function returns, the stream (and file) are closed. *)

val read_char : t -> char

val read_line : buf:Buffer.t -> t -> string
(** Read a line from the stream.
    @param buf a buffer to (re)use. Its content will be cleared. *)

val read_all : buf:Buffer.t -> t -> string
(** Read the whole stream into a string.
    @param buf a buffer to (re)use. Its content will be cleared. *)

val read_until : buf:Buffer.t -> target:string -> t -> unit
(** Advance in the stream until in meet the given target.
    @param buf a buffer to (re)use. Its content will be cleared. *)

val limit_size_to :
  close_rec:bool ->
  max_size:int ->
  too_big:(int -> unit) ->
  t -> t
(* New stream with maximum size [max_size].
   @param close_rec if true, closing this will also close the input stream
   @param too_big called with read size if the max size is reached *)

val read_chunked :
  buf:Buffer.t ->
  fail:(string -> exn) ->
  trailer:(t -> unit) ->
  t -> t
(** Convert a stream into a stream of byte chunks using
    the chunked encoding. The size of chunks is not specified.
    @param buf buffer used for intermediate storage.
    @param fail used to build an exception if reading fails.
*)

val read_exactly :
  close_rec:bool -> size:int -> too_short:(int -> unit) ->
  t -> t
(** [read_exactly ~size bs] returns a new stream that reads exactly
    [size] bytes from [bs], and then closes.
    @param close_rec if true, closing the resulting stream also closes
    [bs]
    @param too_short is called if [bs] closes with still [n] bytes remaining
*)

val read_exactly_bytes :
  too_short:(unit -> unit) -> t -> Bytes.t -> int -> unit

exception FailParse of int
val fail_parse : t -> 'a

val branch_char : (char -> t -> 'a) -> t -> 'a

val exact_char : char -> 'a -> t -> 'a

val exact_string : string -> 'a -> t -> 'a

val star : (t -> unit) -> t -> unit

val plus : (t -> unit) -> t -> unit

val blank : t -> unit

val space : t -> unit

val int : t -> int

val current : t -> string
