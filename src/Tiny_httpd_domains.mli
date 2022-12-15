type status = {
    nb_availables : int Atomic.t;
    nb_connections : int Atomic.t array
  }

val string_status : status -> string

type client = {
    mutable counter : int;
    mutable granularity : int;
    sock : Unix.file_descr;
    status : status;
    domain_id : int;
    mutable connected : bool;
  }

val fake_client : client
(** only for internal use to please qtest *)

val read  : client -> Bytes.t -> int -> int -> int
val write : client -> Bytes.t -> int -> int -> int
val yield : unit -> unit

val run : int -> string -> int -> int -> int -> (client -> unit) ->
            unit Domain.t array
