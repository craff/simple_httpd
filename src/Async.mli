(** General functions related to OCaml's domain and asynchrone cooperative
    multithreading. *)

(** Vocabulary:
    - socket: a file descriptor: may be the connection socket or another ressource
      like a connection to a database, a file etc.
    - client: a connection to the server. Each client has at least one socket (the
      connection socket)
    - session: an application can do several connections to the server and
      be identified as one session using session cookies.
 *)


(** Connection status. Holds the number of clients per domain.  *)
type status = {
    nb_connections : int Atomic.t array
  }

val string_status : status -> string

(** Simple_httpd notion of mutex. You must be careful with server wide mutex:
    a DoS attack could try to hold such a mutex. A mutex per session may be a good
    idea. A mutex per client is useless (client are treated sequentially).

    FIXME: there is a global mutex for file cache. It is holded very shortly and
    once the file is in the cache it is not used anymore, so this is OK.
 *)
module Mutex : sig
  type t

  val create : unit -> t
  val try_lock : t -> bool
  val lock : t -> unit
  val unlock : t -> unit
  val delete : t -> unit
end

type any_continuation (** internal use only *)

(** Type associated to session, user extensible *)
type session_data = ..
type session_data += NoData

(** Record describing clients *)
type client = private {
    id : int;                         (** Unique identifier *)
    mutable connected : bool;         (** Is the client still connected *)
    sock : Unix.file_descr;           (** The sockect for the client *)
    accept_by : int;                  (** index in the listens table of the
                                          adress:port that received the connection *)
    mutable ssl  : Ssl.socket option; (** An eventual ssl context
                                          modified once after ssl negociation *)
    mutable session : session option; (** Session *)
    mutable acont : any_continuation; (** internal use *)
    mutable start_time : float;       (** start of request *)
    mutable locks : Mutex.t list;     (** all lock, locked by this client *)
    buf : Buffer.t;                   (** used to parse headers *)
    mutable read : bytes -> int -> int -> int;
    mutable write : bytes -> int -> int -> int;
    mutable sendfile : Unix.file_descr -> int -> int -> int
  }

and session_info =
  { addr : string
  ; key : string
  ; life_time : float
  ; clients : client list Atomic.t
  ; data : session_data Atomic.t
  ; cleanup : session_data -> unit
  ; mutable last_refresh : float (* protected by mutex_list in Session.ml *)
  ; cookies : (string * string) list Atomic.t
  }

and session = session_info Util.LinkedList.cell

(** only to please qtest *)
val fake_client : client

(** Set the session of a client *)
val set_session : ?session:session -> client -> unit

(** The scheduling primitives *)
val read  : client -> Bytes.t -> int -> int -> int
val write : client -> Bytes.t -> int -> int -> int
val sendfile : client -> Unix.file_descr -> int -> int -> int
val yield : unit -> unit
val sleep : float -> unit
val close : client -> unit
val flush : client -> unit

(** This register the starttime of a request. You may use it to compute
    Request timeout (as opposed to socket timeout which are included
    and reset the timeout if you know a request requires time. *)
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

type socket_type =
  | Io
  | Client
  | Pipe
  | Lock

exception NoRead
exception NoWrite
exception SockError of socket_type * exn
exception ClosedByHandler
exception TimeOut

(** For use if you do not want to use the provided Io module, and want
    to schedule an Io task *)
val schedule_io : Unix.file_descr -> (unit -> int) -> int

val run : nb_threads:int -> listens:Address.t array -> maxc:int ->
          timeout:float -> status:status ->
            (client -> unit) -> unit Domain.t array

val printexn : exn -> string

module Log : sig
  val set_log_requests   : int -> unit
  val set_log_scheduler  : int -> unit
  val set_log_exceptions : int -> unit
  val set_log_folder : ?basename:string -> ?perm:int -> string -> int -> unit
  type log_lvl =
    | Req of int
    | Sch of int
    | Exc of int
  val f : log_lvl ->
          ((('a, out_channel, unit, unit) format4 -> 'a) -> unit) -> unit
end
