
(** {1 Http Server}

    This library implements a very simple, basic HTTP/1.1 server using blocking
    IOs and threads. Basic routing based on {!Scanf} is provided for convenience,
    so that several handlers can be registered.

    It is possible to use a thread pool, see {!create}'s argument [new_thread].

    The [echo] example (see [src/examples/echo.ml]) demonstrates some of the
    features by declaring a few endpoints, including one for uploading files:

    {[
module S = Simple_httpd

let () =
  let server = S.create () in

  (* say hello *)
  S.add_route_handler ~meth:`GET server
    S.Route.(exact "hello" @/ string @/ return)
    (fun name _req -> S.Response.make_string (Ok ("hello " ^name ^"!\n")));

  (* echo request *)
  S.add_route_handler server
    S.Route.(exact "echo" @/ return)
    (fun req -> S.Response.make_string
        (Ok (Format.asprintf "echo:@ %a@." S.Request.pp req)));

  (* file upload *)
  S.add_route_handler ~meth:`PUT server
    S.Route.(exact "upload" @/ string_urlencoded @/ return)
    (fun path req ->
        try
          let oc = open_out @@ "/tmp/" ^ path in
          output_string oc req.S.Request.body;
          flush oc;
          S.Response.make_string (Ok "uploaded file")
        with e ->
          S.Response.fail ~code:500 "couldn't upload file: %s"
            (Printexc.to_string e)
      );

  (* run the server *)
  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
    ]}

    It is then possible to query it using curl:

    {[
$ dune exec src/examples/echo.exe &
listening on http://127.0.0.1:8080

# the path "hello/name" greets you.
$ curl -X GET http://localhost:8080/hello/quadrarotaphile
hello quadrarotaphile!

# the path "echo" just prints the request.
$ curl -X GET http://localhost:8080/echo --data "howdy y'all"
echo:
{meth=GET;
 headers=Host: localhost:8080
         User-Agent: curl/7.66.0
         Accept: */*
         Content-Length: 10
         Content-Type: application/x-www-form-urlencoded;
 path="/echo"; body="howdy y'all"}


    ]}

*)


(** {2 Generic stream of data}

    Streams are used to represent a series of bytes that can arrive progressively.
    For example, an uploaded file will be sent as a series of chunks. *)

module Byte_stream = Simple_httpd_stream

(** {2 Main Server Type} *)

(** @inline *)
include module type of struct include Simple_httpd_server end

(** {2 Utils} *)

module Util = Simple_httpd_util

(** {2 Static directory serving} *)

module Dir = Simple_httpd_dir

module Html = Simple_httpd_html
(** Alias to {!Simple_httpd_html} *)

module Domain = Simple_httpd_domain

module Session = Simple_httpd_session

(** Type describing addresses we want to listen too, provided
    here to avoid module opening *)
type listenning = Simple_httpd_domain.listenning =
  {
    addr : string;
    port : int;
    ssl  : Ssl.context option ;
  }

(** {2 cooperative threading *)

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
