
val read : Unix.file_descr -> Bytes.t -> int -> int -> int
val write : Unix.file_descr -> string -> int -> int -> int

val run : int -> string -> int -> int -> (Unix.file_descr -> unit) ->
            unit Domain.t array
