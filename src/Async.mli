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

module Semaphore : sig
  type t
  val create : int -> t
  val decr : t -> unit
  val try_decr : t -> bool
  val incr : t -> unit
  val delete : t -> unit
end

type any_continuation (** internal use only *)

(** Record describing clients *)
type client = private {
    id : int;                         (** Unique identifier *)
    mutable connected : bool;         (** Is the client still connected *)
    sock : Unix.file_descr;           (** The sockect for the client *)
    peer : string;                    (** peer IP *)
    accept_by : int;                  (** index in the listens table of the
                                          adress:port that received the connection *)
    mutable ssl  : Ssl.socket option; (** An eventual ssl context
                                          modified once after ssl negociation *)
    mutable cont : bool;              (** Read the next request *)
    mutable session : session option; (** Session *)
    mutable acont : any_continuation; (** internal use *)
    mutable start_time : float;       (** start of request *)
    mutable timeout_ref : float;      (** reference for timeout, usually start_time,
                                          except if [Async.reset_timeout] is used*)
    mutable locks : Mutex.t list;     (** all lock, locked by this client *)
    buf : Buffer.t;                   (** used to parse headers *)
    mutable last_seen_cell : client Util.LinkedList.cell;
    (** pointer to the linked list used to detect timeout *)
    close : unit -> unit              (** close the client *)
  }

and session_info =
  { addr : string
  ; key : string
  ; life_time : float
  ; clients : client list Atomic.t
  ; data : Key.data Atomic.t
  ; mutable cell : session_info Util.LinkedList.cell
  ; mutable last_refresh : float (* protected by mutex_list in Session.ml *)
  }

and session = session_info Util.LinkedList.cell

module Client : sig
  type t = client
  val connected : t -> bool
  val peer : t -> string
  val start_time : t -> float
  val is_ssl : t -> bool
  val close : t -> unit
  val ssl_flush : t -> unit
end

(** only to please qtest *)
val fake_client : client

(** close the client *)
val close : client -> unit

(** close all clients *)
val close_all : int -> unit

(** Set the session of a client *)
val set_session : ?session:session -> client -> unit

(** The scheduling primitives *)
val read  : client -> Bytes.t -> int -> int -> int
val write : client -> Bytes.t -> int -> int -> int
val sendfile : client -> Unix.file_descr -> int -> int -> int
val yield : unit -> unit
val sleep : float -> unit

(** This register the starttime of a request. You may use it to compute
    Request timeout (as opposed to socket timeout) *)
val register_starttime : client -> float

(** reset the timeout if you know a request requires time. *)
val reset_timeout : client -> unit

(** Tell the client not to try to read the next request *)
val stop_client : client -> unit

(** Module with function similar to Unix.read and Unix.single_write
    but that will perform scheduling *)
module Io : sig
  type t

  val create : ?finalise:(unit -> unit) -> Unix.file_descr -> t
  val close : t -> unit
  val read : t -> Bytes.t -> int -> int -> int
  val write : t -> Bytes.t -> int -> int -> int

  val formatter : t -> Format.formatter

  val poll : ?edge_trigger:bool ->
             ?read:bool ->
             ?write:bool ->
             Unix.file_descr -> unit
end

type socket_type =
  | Io
  | Fd
  | Client
  | Pipe
  | Lock
  | Decr

exception NoRead
exception NoWrite
exception ClosedByHandler
exception TimeOut

(** For use if you do not want to use the provided Io module, and want
    to schedule an Io task *)
val schedule_io : Unix.file_descr -> (unit -> int) -> int

val run : nb_threads:int -> listens:Address.t array -> maxc:int ->
          timeout:float -> set_domains:(Domain.id array -> unit) ->
            (client -> unit) -> unit Domain.t array

val printexn : exn -> string

module Log : sig
  val set_log_requests        : int -> unit
  val set_log_scheduler       : int -> unit
  val set_log_exceptions      : int -> unit
  val set_log_authentications : int -> unit
  val set_log_processes       : int -> unit
  val set_log_user            : int -> unit

  val set_log_folder : ?basename:string -> ?perm:int -> string -> int -> unit
  type log_lvl =
    | Req of int
    | Sch of int
    | Exc of int
    | Aut of int
    | Prc of int
    | Usr of int
  val f : log_lvl ->
          ((('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) -> unit

  val log_folder : string ref
  val log_basename : string ref
  val log_perm : int ref

  val fname : int -> string

  val init_log_folder : int -> unit
end

type socket_infos
type pollResult

type domain_info =
  { mutable cur_client : client (* the client currently running *)
  ; pendings : socket_infos array
  ; poll_list : Polly.t
  ; bytes : Bytes.t (* a preallocated buffer *)
  ; last_seen : client Util.LinkedList.t
  ; ready : pollResult Queue.t
  ; nb_connections : int Atomic.t (* -1 for the accepting domain, -2 for unused domain *)
  }

val all_domain_info : domain_info array
