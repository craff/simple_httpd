type client = {
    mutable counter : int;
    mutable granularity : int; (* TODO adjust dynamically *)
    sock : Unix.file_descr
  }

val read  : client -> Bytes.t -> int -> int -> int
val write : client -> string -> int -> int -> int

val run : int -> string -> int -> int -> int -> (client -> unit) ->
            unit Domain.t array
