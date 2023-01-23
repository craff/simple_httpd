(** General functions related to OCaml's domain and asynchrone cooperative
    multithreading. *)

(** Vocabulary:
    - socket: a file descriptor: may be the connection socket or another ressource
      like a connection to a database, a file etc.
    - client: a connection to the server. Each client has at least one socket (the
      connection socket)
    - session: an application can do several connection to the server and
      be identifies as one session using session cookies.
 *)

(** Connection status. Holds the number of clients per domain.  We support
    several simultaneous server: each server would have its own status.  *)
type status = {
    nb_connections : int Atomic.t array
  }

val string_status : status -> string

(** Type associated to session, user extensible *)
type session_data = ..
type session_data += NoData

(** Simple_httpd notion of mutex. It is a bad idea to have server wide mutex:
    a DoS attack could try to hold such a mutex. A mutex per session may be a good
    idea. A mutex per client is useless (client are treated sequentially.

    FIXME: there is a global mutex for file cache. It is hold very shortly and
    once the file is in the cache it is not used anymore, so this is OK.
*)

module Mutex : sig
  type t

  val create : unit -> t
  val try_lock : t -> bool
  val lock : t -> unit
  val unlock : t -> unit

  (** Wait for a boolean but do not lock anything *)
  val wait_bool : bool Atomic.t -> unit
end

type any_continuation (** internal use only *)

(** Record describing clients *)
type client = {
    id : int;                         (** Unique identifier *)
    mutable connected : bool;         (** Is the client still connected *)
    sock : Unix.file_descr;           (** The sockect for the client *)
    mutable ssl  : Ssl.socket option; (** An eventual ssl context
                                          modified once after ssl negociation *)
    mutable session : session option; (** Session *)
    mutable acont : any_continuation; (** internal use *)
    mutable start_time : float;       (** start of request *)
    buf : Buffer.t                    (** used to parse headers *)
  }

(** Record describing sessions *)
and session =
  { addr : string
  ; key : string
  ; mutex : Mutex.t
  ; mutable clients : client list
  ; mutable data : session_data
  }

(** only to please qtest *)
val fake_client : client

(** The scheduling primitives *)
val read  : client -> Bytes.t -> int -> int -> int
val write : client -> Bytes.t -> int -> int -> int
val yield : unit -> unit
val sleep : float -> unit
val close : client -> unit
val flush : client -> unit

(** This register the starttime of a request to compute timeout.
    You could use it if you know a request require time and you
    want to avoit timeout *)
val register_starttime : client -> unit

(** Module with function similar to Unix.read and Unix.single_write
    but that will perform scheduling *)
module type Io = sig
  type t

  val create : Unix.file_descr -> t
  val close : t -> unit
  val read : t -> Bytes.t -> int -> int -> int
  val write : t -> Bytes.t -> int -> int -> int
end

module Io : Io

type socket_type = Io of Io.t | Client of client | Pipe

exception NoRead
exception NoWrite
exception SockError of socket_type * exn
exception ClosedByHandler
exception TimeOut

(** For use if you do not want to use the provided Io module, and want
    to schedule an Io task *)
val schedule_io : Unix.file_descr -> (unit -> int) -> int

(** Type describing a socket we listen for *)
type listenning = {
    addr : string;
    port : int;
    ssl  : Ssl.context option ;
  }

val run : nb_threads:int -> listens:listenning list -> maxc:int ->
          delta:float -> timeout:float -> status:status ->
            (client -> unit) -> unit Domain.t array

val printexn : exn -> string
