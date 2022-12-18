type status = {
    nb_availables : int Atomic.t;
    nb_connections : int Atomic.t array
  }

val string_status : status -> string

type client = {
    mutable connected : bool;
    mutable counter : int;
    mutable granularity : int;
    sock : Unix.file_descr;
    ssl  : Ssl.socket option;
    status : status;
    domain_id : int;
  }

val fake_client : client
(** only for internal use to please qtest *)

val read  : client -> Bytes.t -> int -> int -> int
val write : client -> Bytes.t -> int -> int -> int
val yield : unit -> unit
val sleep : float -> unit
val close : client -> unit
val flush : client -> unit

type listenning = {
    addr : string;
    port : int;
    ssl  : Ssl.context option ;
  }

val run : nb_threads:int -> listens:listenning list -> maxc:int ->
          granularity:int -> timeout:float -> (client -> unit) ->
            unit Domain.t array
