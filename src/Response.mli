(** {1 Responses}

    Responses are what a http server, such as {!Simple_httpd}, send back to
    the client to answer a {!Request.t}*)

type body = String of string
          | Stream of  { inp : Input.t
                       ; synch : (unit -> unit) option
                       ; size : int option (** use chunk encoding only
                                               if size is not provided *)
                       ; close : Input.t -> unit}
                (** flush each part and call f for each part if second arg is [Some f],
                    use the close function at end of output *)
          | File of
              { fd : Util.Sfd.t
              ; size : int
              ; close : Util.Sfd.t -> unit
                (** if using sendfile, one might want to maintain the fd open
                    for another request, sharing file descriptor would limit
                    the number of open files *)}
          | Void
(** Body of a response, either as a simple string,
    or a stream of bytes, a file or nothing (for server-sent events). *)

type t = { code: Response_code.t (** HTTP response code. See {!Response_code}. *)
         ; headers: Headers.t    (** Headers of the reply. Some will be set by [Simple_httpd] automatically. *)
         ; body: body            (** Body of the response. Can be empty. *)
         ; post: unit -> unit    (** A function called after sending the response. *)
         }
(** A response to send back to a client. *)

val body : t -> body
(** Get the body of the response *)

val set_body : body -> t -> t
(** Set the body of the response. *)

val set_header : Headers.header -> string -> t -> t
(** Set a header. *)

val update_headers : (Headers.t -> Headers.t) -> t -> t
(** Modify headers *)

val set_headers : Headers.t -> t -> t
(** Set all headers. *)

val headers : t -> Headers.t
(** Get headers *)

val set_code : Response_code.t -> t -> t
(** Set the response code. *)

val set_post : (unit -> unit) -> t -> t
(** Set the post function, run after sending the response *)

val get_post : t -> (unit -> unit)
(** Get the post function, run after sending the response *)

val make_raw :
  ?cookies:Cookies.t ->
  ?headers:Headers.t ->
  code:Response_code.t ->
  string ->
  t
(** Make a response from its raw components, with a string body.
    Use [""] to not send a body at all. *)

val make_raw_stream :
  ?synch:(unit->unit) ->
  ?close:(Input.t->unit) ->
  ?cookies:Cookies.t ->
  ?headers:Headers.t ->
  code:Response_code.t ->
  ?size:int ->
  Input.t ->
  t
(** Same as {!make_raw} but with a stream body. The body will be sent with
    the chunked transfer-encoding. *)

val make_raw_file :
  ?cookies:Cookies.t ->
  ?headers:Headers.t ->
  ?close:(Util.Sfd.t -> unit) ->
  code:Response_code.t ->
  int -> Util.Sfd.t ->
  t
(** Same as {!make_raw} but with a file_descriptor. The body will be sent with
    Linux sendfile system call.
    @param [close] tells if one must close the file_descriptor after sending
      the response.
 *)

val make :
  ?cookies:Cookies.t ->
  ?headers:Headers.t ->
  body -> t
(** [make r] turns a body into a response.

    - [make (Ok body)] replies with [200] and the body.
    - [make (Error (code,msg))] replies with the given error code
    and message as body.
 *)

val make_void :
  ?cookies:Cookies.t ->
  ?headers:Headers.t ->
  code:Response_code.t -> unit -> t

val make_string :
  ?cookies:Cookies.t ->
  ?headers:Headers.t ->
  string -> t
(** Same as {!make} but with a string body. *)

val make_stream :
  ?synch:(unit->unit) ->
  ?close:(Input.t->unit) ->
  ?cookies:Cookies.t ->
  ?headers:Headers.t ->
  ?size:int ->
  Input.t -> t
(** Same as {!make} but with a stream body. *)

val make_file :
  ?cookies:Cookies.t ->
  ?headers:Headers.t ->
  ?close:(Util.Sfd.t -> unit) ->
  int ->
  Util.Sfd.t -> t
(** Same as {!make} but with a file_descr body. *)

val fail :
  ?cookies:Cookies.t ->
  ?headers:Headers.t ->
  code:Response_code.t ->
  ('a, unit, string, t) format4 -> 'a
(** Make the current request fail with the given code and message.
    Example: [fail ~code:404 "oh noes, %s not found" "waldo"].
 *)

val fail_raise : ?headers:Headers.t -> ?cookies:Cookies.t ->
                 code:Response_code.t -> ('a, unit, string, 'b) format4 -> 'a
(** Similar to {!fail} but raises an exception that exits the current handler.
    This should not be used outside of a (path) handler.
    Example: [fail_raise ~code:404 "oh noes, %s not found" "waldo"; never_executed()]
 *)

val pp : Format.formatter -> t -> unit
(** Pretty print the response. *)

(** internal use *)
val output_ : Method.t -> Output.t -> t -> unit
