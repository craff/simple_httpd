
val read : Unix.file_descr -> Bytes.t -> int -> int -> int
val write : Unix.file_descr -> string -> int -> int -> int
val send : Unix.file_descr -> unit
val run : int -> (Unix.file_descr -> unit) -> unit Domain.t array
