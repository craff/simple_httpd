type status = {
    nb_availables : int Atomic.t;
    nb_connections : int Atomic.t array
  }

type client = {
    mutable counter : int;
    mutable granularity : int;
    sock : Unix.file_descr;
    status : status;
  }

val read  : client -> Bytes.t -> int -> int -> int
val write : client -> Bytes.t -> int -> int -> int

val run : int -> string -> int -> int -> int -> (client -> unit) ->
            unit Domain.t array
