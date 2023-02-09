(** {1 Main module for Simple_httpd} *)

(** @inline *)
include module type of struct include Simple_httpd_server end

(** {2 Generic stream of data}

    Streams are used to represent a series of bytes that can arrive progressively.
    For example, an uploaded file will be sent as a series of chunks and also
    for output streams. *)

module Input = Simple_httpd_input

module Output = Simple_httpd_output

module Util = Simple_httpd_util

(** {2 Static directory serving} *)

module Dir = Simple_httpd_dir

module Html = Simple_httpd_html
(** Alias to {!Simple_httpd_html} *)

module Domain = Simple_httpd_domain

module Session = Simple_httpd_session

(** {2 cooperative threading} *)

(** The following functions deals with cooperative multi-tasking on each
    domain.  First, recall that the OS will choose via [select] on which
    domain you serve your client, and after that, it is impossible for a job
    to change domain. This is a current limitation of domain with OCaml 5.0.

    Then, on each domain, priority is based on arrival time: first arrived,
    first to run. With one exception: Mutexes have the highest priority,
    and among mutexes, the highest priority is again, first arrived, first
    to run.

    If you want to have mutex with lower priority, you may do:
    [ yield (); lock m ] or
    [ if not (Mutex.try_lock m) then (yield (); lock m)]

    This should more or less give a priority for mutexes comparable to other
    scheduled jobs... With more risk of deadlock ?

    Normally context switching occurs when read or write is blocked in the
    socket serving the client or regularly (based on granularity parameter)
*)

val yield : unit -> unit
(** let other thread run. Should be called for treatment that take time
    before sending results or reading data and when the other primitive
    can not be used. This happends typically for a pure computing task.
    Other solutions exists for instance for database request.
*)

(*val schedule : unit -> unit*)
(** same a yield, but only let other job run if we are running for more
    than delta seconds (a parameter of the server *)

val sleep : float -> unit
(** Same as above, but with a minimum sleeping time in second *)


(** Module with function similar to Unix.read and Unix.single_write
    but that will perform scheduling *)
module Io : Simple_httpd_domain.Io

(** The two function below are provided if you do not want to use the Io module. *)

(** [schedule_io sock action] should be called when a non blocking read/write
    operation would have blocked. When read become possible, [action ()] will
    be called.  The return value should be (if possible) the number of bytes
    read or written. It this is meaningless, return a non zero value if some
    progress was made, while returning 0 will call [close Closed].

    A typical application for this is when interacting with a data base in non
    blocking mode. For just reading a file or socket, use the Io module above.
  *)
val schedule_io : Unix.file_descr -> (unit -> int) -> int

module Mutex : sig
  type t

  val create : unit -> t
  val try_lock : t -> bool
  val lock : t -> unit
  val unlock : t -> unit
end
