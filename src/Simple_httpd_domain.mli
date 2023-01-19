(** General functions related to OCaml's domain and asynchrone cooperative
    multithreading. *)

type status = {
    nb_availables : int Atomic.t;
    nb_connections : int Atomic.t array
  }

val string_status : status -> string

type session_data = ..
type session_data += NoData

module Mutex : sig
  type t

  val create : unit -> t
  val try_lock : t -> bool
  val lock : t -> unit
  val unlock : t -> unit
end

type any_continuation =
    N : any_continuation
  | C : ('a,unit) Effect.Deep.continuation -> any_continuation

type client = {
    mutable connected : bool;
    sock : Unix.file_descr;
    mutable ssl  : Ssl.socket option; (* modified once after ssl negociation *)
    status : status;
    domain_id : int;
    mutable session : session option;
    mutable acont : any_continuation;
    id : int
  }

and session =
  { addr : string
  ; key : string
  ; mutex : Mutex.t
  ; mutable clients : client list
  ; mutable data : session_data
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

(** Module with function similar to Unix.read and Unix.single_write
    but that will perform scheduling *)

module type Io = sig
  type t

  val create : Unix.file_descr -> t
  val close : t -> unit
  val close_no_error : t -> unit
  val read : t -> Bytes.t -> int -> int -> int
  val write : t -> Bytes.t -> int -> int -> int
end

module Io : Io

type socket_type = Io of Io.t | Client of client

exception NoRead
exception NoWrite
exception SockError of socket_type * exn
exception ClosedByHandler
exception TimeOut

val schedule_read : Unix.file_descr -> (unit -> int) -> int

val schedule_write : Unix.file_descr -> (unit -> int) -> int

val run : nb_threads:int -> listens:listenning list -> maxc:int ->
          delta:float -> timeout:float -> (client -> unit) ->
            unit Domain.t array

val printexn : exn -> string
