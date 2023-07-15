(* included in main module Simple_httpd *)

type buf = Buffer.t                     (** type alias needed below *)

(** {1 Main Server type} *)

type t
(** A HTTP server. See {!create} for more details. *)

module type Parameters = sig
  val max_connections : int ref
  val num_threads : int ref
  val timeout : float ref
  val buf_size : int ref
  val ssl_reload_period : int -> unit

  val log_requests : int ref
  val log_exceptions : int ref
  val log_scheduler : int ref
  val log_folder : string ref
  val log_basename : string ref
  val log_perm : int ref
end

val args : unit -> (Arg.key * Arg.spec * Arg.doc) list * (module Parameters)

val create :  ?listens:Address.t list -> (module Parameters) -> t

(** Create a new webserver.

    The server will not do anything until {!run} is called on it. Before starting the server, one can use {!add_route_handler} to specify how to handle incoming requests.

    @param masksigpipe if true, block the signal [Sys.sigpipe] which otherwise tends to kill client threads when they try to write on broken sockets. Default: [true].
    @param buf_size size for buffers (since 0.11)
    @param max_connections maximum number of simultaneous connections.
    @param num_thread number of thread to treat client.
    @param timeout connection is closed if the socket does not do read or
      write for the amount of second. Default: 300s, (< 0.0 means no timeout).
      timeout is not recommended when using proxy.
    @param addr address (IPv4 or IPv6) to listen on. Default ["127.0.0.1"].
    @param port to listen on. Default [8080].
    @param sock an existing socket given to the server to listen on, e.g. by
      systemd on Linux (or launchd on macOS). If passed in, this socket will be
      used instead of the [addr] and [port]. If not passed in, those will be
      used. This parameter exists since 0.10.
*)

val listens : t -> Address.t array
(** Addresses and ports on which the server listens. *)

val status : t -> Async.status
(** Returns server status *)

val active_connections : t -> int
(** Number of active connections *)

(** {1 Route handlers}

    Here are the main function to explain what you server should to depending
    on the url send by the client.
*)

val add_route_handler :
  ?addresses:Address.t list ->
  ?hostnames:string list ->
  ?meth:Method.t ->
  ?filter:Input.t Route.Filter.t ->
  t ->
  ('a, string Request.t -> Response.t) Route.t -> 'a ->
  unit
(** [add_route_handler server route f] add a route to give a [string] as
    response.

    For instance, [add_route_handler serverRoute.(exact "path" @/ string @/
    int @/ return) f] calls [f "foo" 42 request] when a [request] with path
    "path/foo/42/" is received.

    Note that the handlers are called in the following precision order:
    - {!Route.return}, accepting only the empty url is the most precide
    - [{!Route.exact} s], is the second, tried
    - {!Route.int}
    - {!Route.string}
    - {!Route.rest} is tried last.
    - In case of ambiguity, the first added route is tried first.

    @param adresses if provided, only accept requests from the given
    adress and port. Will raise
       [Invalid_argument "add_route: the server is not listening to that adress"]
    if the server is not listenning to that adresse and port.
    @param meth if provided, only accept requests with the given method.
    Typically one could react to [`GET] or [`PUT].
    @param filter can be used to modify the request and response and also
    to reject some request using {!Response.fail_raise}. The default filter
    accept all requests and does not do any transformation.
*)

val add_route_handler_stream :
  ?addresses:Address.t list ->
  ?hostnames:string list ->
  ?meth:Method.t ->
  ?filter:Input.t Route.Filter.t ->
  t ->
  ('a, Input.t Request.t -> Response.t) Route.t -> 'a ->
  unit
(** Similar to {!add_route_handler}, but where the body of the request
    is a stream of bytes that has not been read yet.
    This is useful when one wants to stream the body directly into a parser,
    json decoder (such as [Jsonm]) or into a file. *)

(** {1 Server-sent events}

    {b EXPERIMENTAL}: this API is not stable yet. *)

(** A server-side function to generate of Server-sent events.

    See {{: https://html.spec.whatwg.org/multipage/server-sent-events.html} the w3c page}
    and {{: https://jvns.ca/blog/2021/01/12/day-36--server-sent-events-are-cool--and-a-fun-bug/}
    this blog post}.
  *)
module type SERVER_SENT_GENERATOR = sig
  val set_headers : Headers.t -> unit
  (** Set headers of the response.
      This is not mandatory but if used at all, it must be called before
      any call to {!send_event} (once events are sent the response is
      already sent too). *)

  val send_event :
    ?event:string ->
    ?id:string ->
    ?retry:string ->
    data:string ->
    unit -> unit
  (** Send an event from the server.
      If data is a multiline string, it will be sent on separate "data:" lines. *)

  val close : unit -> unit
  (** Close connection. *)
end

type server_sent_generator = (module SERVER_SENT_GENERATOR)
(** Server-sent event generator *)

val add_route_server_sent_handler :
  ?addresses:Address.t list ->
  ?hostnames:string list ->
  ?filter:Input.t Route.Filter.t ->
  t ->
  ('a, string Request.t -> server_sent_generator -> unit) Route.t -> 'a ->
  unit
(** Add a handler on an endpoint, that serves server-sent events.

    The callback is given a generator that can be used to send events
    as it pleases. The connection is always closed by the client,
    and the accepted method is always [GET].
    This will set the header "content-type" to "text/event-stream" automatically
    and reply with a 200 immediately.
    See {!server_sent_generator} for more details.

    This handler stays on the original thread (it is synchronous). *)

(** {1 Run the server} *)

val run : t -> unit
(** Run the main loop of the server, listening on a socket
    described at the server's creation time. *)
