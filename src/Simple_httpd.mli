(** @inline *)
include module type of struct include Simple_httpd_server end

(** {1 Static directory serving and page generation} *)


(** The following two modules allows to server static files (possibly using
    a memory cache (automatically updated) or a virtual file system.
    These are not much faster (would even be slower if we used [sendfile] Linux
    system call), but it saves using sockets which are a limited ressource.
*)
module Dir = Simple_httpd_dir

(** A module to generate static html that is reasonnably fast and does not
    requires ppx while still remaining relatively readable *)
module Html = Simple_httpd_html

(** {1 Cooperative threading} *)

(** The following functions deals with cooperative multi-tasking on each
    domain.  First, recall {!Simple_httpd} will choose the domain with the
    least number of clients to serve a new client, and after that, it is
    impossible for a job to change domain. This is a current limitation of
    domain with OCaml 5.0.

    Then, on each domain, priority is based on arrival time: first arrived,
    first to run.

    Normally context switching occurs when read or write is blocked or when a
    mutex is already locked, with one exception: {!yield} is called after each
    request treatment if the client uses [keepalive] connection.
*)

val yield : unit -> unit
(** let other thread run. Should be called for treatment that take time
    before sending results or reading data and when the other primitives
    can not be used. This happends typically for a pure computing task.
    Other solutions exists for instance for database request.
*)

val sleep : float -> unit
(** Same as above, but with a minimum sleeping time in second *)

(** Module with function similar to Unix.read and Unix.single_write
    but that will perform scheduling when blocking. This can be used to
    access your database. It has been tested with OCaml's bindings to [libpq]. *)
module Io : Simple_httpd_domain.Io

(** The two functions below are provided if you do not want to use the Io module. *)

(** [schedule_io sock action] should be called when a non blocking read/write
    operation would have blocked. When read become possible, [action ()] will
    be called.  The return value should be (if possible) the number of bytes
    read or written. It this is meaningless, return a non zero value if some
    progress was made, while returning 0 will call [close Closed] and terminates
    the management of the client.

    A typical application for this is when interacting with a data base in non
    blocking mode. For just reading a socket, use the Io module above.
  *)
val schedule_io : Unix.file_descr -> (unit -> int) -> int

(** This small module provide Mutexes to protect data used by several clients,
   with the usual semantics. They are implemented using Linux [eventfd] *)
module Mutex : sig
  type t

  val create : unit -> t
  val try_lock : t -> bool
  val lock : t -> unit
  val unlock : t -> unit
end

(** {1 Session managment} *)

(** This module allows to mange session which are common to several client
    and can survive a deconnection of the clients. This do not provide
    any form of authentication, but it is easy to use them to implement
    authentication. *)
module Session = Simple_httpd_session

(** {1 Lower level modules} *)


(** Input streams are used to represent a series of bytes that can arrive
    progressively.  For example, an uploaded file will be sent as a series of
    chunks and also for output streams. *)
module Input = Simple_httpd_input

(** Output stream *)
module Output = Simple_httpd_output

(** Module with Simple_httpd's scheduler, most useful function have already
    be presented abovve *)
module Domain = Simple_httpd_domain

(** Some tools, like url encoding *)
module Util = Simple_httpd_util
