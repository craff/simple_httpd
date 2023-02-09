
(** HTTP server.

    This module implements a very simple, basic HTTP/1.1 server using blocking
    IOs and threads.

    @since NEXT_RELEASE
*)

(** {2 Methods}

    A short module defining the various HTTP methods (GET,PUT,...)*)

module Meth : sig
  type t =
    | GET
    | PUT
    | POST
    | HEAD
    | DELETE
  (** A HTTP method.
      For now we only handle a subset of these.

      See https://tools.ietf.org/html/rfc7231#section-4 *)

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end

(** {2 Cookies}

    Cookies are data that are maintend both on server and clients.
    This is a module to get and set cookies in the headers. *)

module Cookies : sig
  type t

  val empty : t
  val parse : string -> t
  val add : string -> Http_cookie.t -> t -> t
  val create : ?path:string ->
      ?domain:string ->
      ?expires:Http_cookie.date_time ->
      ?max_age:int64 ->
      ?secure:bool ->
      ?http_only:bool ->
      ?same_site:Http_cookie.same_site ->
      ?extension:string ->
      name:string ->
      string -> t -> t

  val get : string -> t -> Http_cookie.t

  (** remove a cookie by setting a negative max-age. Does nothing
      if there are no cookie with that name. *)
  val delete : string -> t -> t

  (** remove all cookies by setting a negative max-age *)
  val delete_all : t -> t
end

(** {2 Headers}

    Headers are metadata associated with a request or response. This module provide
    the necessary function to read and modify headers *)

(** A module defining all the legal header names *)

module Headers : sig
  (** @inline *)
  include module type of Simple_httpd_header

  type t = (header * string) list
  (** The header files of a request or response.

      Neither the key nor the value can contain ['\r'] or ['\n'].
      See https://tools.ietf.org/html/rfc7230#section-3.2 *)

  val empty : t
  (** Empty list of headers
      @since 0.5 *)

  val get : ?f:(string->string) -> header -> t -> string option
  (** [get k headers] looks for the header field with key [k].
      @param f if provided, will transform the value before it is returned. *)

  val set : header -> string -> t -> t
  (** [set k v headers] sets the key [k] to value [v].
      It erases any previous entry for [k] *)

  val set_cookies : Cookies.t -> t -> t
  (** Encode all the cookies in the header *)

  val remove : header -> t -> t
  (** Remove the key from the headers, if present. *)

  val contains : header -> t -> bool
  (** Is there a header with the given key? *)

  val pp : Format.formatter -> t -> unit
  (** Pretty print the headers. *)
end

(** {2 Requests}

    Requests are sent by a client, e.g. a web browser, curl or wget. *)

type buf = Buffer.t                     (** type alias needed below *)

type byte_stream = Simple_httpd_input.t (** type alias needed below *)

module Request : sig
  type 'body t = private {
    meth: Meth.t;
    host: string;
    client: Simple_httpd_domain.client;
    headers: Headers.t;
    cookies: Cookies.t;
    http_version: int*int;
    path: string;
    path_components: string list;
    query: (string*string) list;
    body: 'body;
    start_time: float;
    trailer: (Headers.t * Cookies.t) option ref;
  }
  (** A request with method, path, host, headers, and a body, sent by a client.

      The body is polymorphic because the request goes through
      several transformations. First it has no body, as only the request
      and headers are read; then it has a stream body; then the body might be
      entirely read as a string via {!read_body_full}.

      @since 0.6 The field [query] was added and contains the query parameters in ["?foo=bar,x=y"]
      @since 0.6 The field [path_components] is the part of the path that precedes [query] and is split on ["/"].
      @since 0.11 the type is a private alias
      @since 0.11 the field [start_time] was added
      @since simple_httpd: [trailer] are automatically updated when finishing to read a chunker body
  *)

  val pp : Format.formatter -> string t -> unit
  (** Pretty print the request and its body *)

  val pp_ : Format.formatter -> _ t -> unit
  (** Pretty print the request without its body *)

  val headers : _ t -> Headers.t
  (** List of headers of the request, including ["Host"] *)

  val get_header : ?f:(string->string) -> _ t -> Headers.header -> string option

  val get_header_int : _ t -> Headers.header -> int option

  val set_header : Headers.header -> string -> 'a t -> 'a t
  (** [set_header k v req] sets [k: v] in the request [req]'s headers. *)

  val update_headers : (Headers.t -> Headers.t) -> 'a t -> 'a t
  (** Modify headers
      @since 0.11 *)

  val set_body : 'a -> _ t -> 'a t
  (** [set_body b req] returns a new query whose body is [b].
      @since 0.11 *)

  val cookies : _ t -> Cookies.t
  (** List of cookies of the request
      @since 0.12 *)

  val get_cookie : _ t -> string -> Http_cookie.t option
  (** get a cookie
      @since 0.12 *)

  val host : _ t -> string
  (** Host field of the request. It also appears in the headers. *)

  val meth : _ t -> Meth.t
  (** Method for the request. *)

  val path : _ t -> string
  (** Request path. *)

  val client : _ t -> Simple_httpd_domain.client
  (** Request client *)

  val query : _ t -> (string*string) list
  (** Decode the query part of the {!field-path} field
      @since 0.4 *)

  val body : 'b t -> 'b
  (** Request body, possibly empty. *)

  val start_time : _ t -> float
  (** time stamp (from [Unix.gettimeofday]) after parsing the first line of the request
      @since 0.11 *)

  val trailer : _ t -> (Headers.t * Cookies.t) option
  (** trailer, read after a chunked body. Only maeningfull after the body stream
      we fully read and closed *)

  val limit_body_size : max_size:int -> byte_stream t -> byte_stream t
  (** Limit the body size to [max_size] bytes, or return
      a [413] error.
      @since 0.3
  *)

  val read_body_full : buf:Buffer.t -> byte_stream t -> string t
  (** Read the whole body into a string. Potentially blocking.

      @param buf_size initial size of underlying buffer (since 0.11) *)

  (**/**)
  (* for testing purpose, do not use *)
  module Internal_ : sig
    val parse_req_start : buf:buf -> client:Simple_httpd_domain.client ->
                          get_time_s:(unit -> float) -> byte_stream -> byte_stream t option
    val parse_body : buf:buf -> byte_stream t -> byte_stream t
  end
  (**/**)
end

(** {2 Response Codes}

    Response code allows client to know if a request failed and give a reason.
    This module is not complete (yet). *)

module Response_code : sig
  type t = int
  (** A standard HTTP code.

      https://tools.ietf.org/html/rfc7231#section-6 *)

  val ok : t
  (** The code [200] *)

  val not_found : t
  (** The code [404] *)

  val descr : t -> string
  (** A description of some of the error codes.
      NOTE: this is not complete (yet). *)
end

(** {2 Responses}

    Responses are what a http server, such as {!Simple_httpd}, send back to
    the client to answer a {!Request.t}*)

module Response : sig
  type body = String of string
            | Stream of byte_stream
            | Void
  (** Body of a response, either as a simple string,
      or a stream of bytes, or nothing (for server-sent events). *)

  type t = private
    { code: Response_code.t (** HTTP response code. See {!Response_code}. *)
    ; headers: Headers.t    (** Headers of the reply. Some will be set by [Simple_httpd] automatically. *)
    ; body: body            (** Body of the response. Can be empty. *)
  }
  (** A response to send back to a client. *)

  val set_body : body -> t -> t
  (** Set the body of the response.
      @since 0.11 *)

  val set_header : Headers.header -> string -> t -> t
  (** Set a header.
      @since 0.11 *)

  val update_headers : (Headers.t -> Headers.t) -> t -> t
  (** Modify headers
      @since 0.11 *)

  val set_headers : Headers.t -> t -> t
  (** Set all headers.
      @since 0.11 *)

  val headers : t -> Headers.t
  (** Get headers
      @since Simple_httpd *)

  val set_code : Response_code.t -> t -> t
  (** Set the response code.
      @since 0.11 *)

  val make_raw :
    ?cookies:Cookies.t ->
    ?headers:Headers.t ->
    code:Response_code.t ->
    string ->
    t
  (** Make a response from its raw components, with a string body.
      Use [""] to not send a body at all. *)

  val make_raw_stream :
    ?cookies:Cookies.t ->
    ?headers:Headers.t ->
    code:Response_code.t ->
    byte_stream ->
    t
  (** Same as {!make_raw} but with a stream body. The body will be sent with
      the chunked transfer-encoding. *)

  val make :
    ?cookies:Cookies.t ->
    ?headers:Headers.t ->
    body -> t
  (** [make r] turns a body into a response.

      - [make (Ok body)] replies with [200] and the body.
      - [make (Error (code,msg))] replies with the given error code
        and message as body.
  *)

  val make_string :
    ?cookies:Cookies.t ->
    ?headers:Headers.t ->
    string -> t
  (** Same as {!make} but with a string body. *)

  val make_stream :
    ?cookies:Cookies.t ->
    ?headers:Headers.t ->
    byte_stream -> t
  (** Same as {!make} but with a stream body. *)

  val fail :
    ?cookies:Cookies.t ->
    ?headers:Headers.t -> code:int ->
    ('a, unit, string, t) format4 -> 'a
  (** Make the current request fail with the given code and message.
      Example: [fail ~code:404 "oh noes, %s not found" "waldo"].
  *)

  val fail_raise : code:int -> ('a, unit, string, 'b) format4 -> 'a
  (** Similar to {!fail} but raises an exception that exits the current handler.
      This should not be used outside of a (path) handler.
      Example: [fail_raise ~code:404 "oh noes, %s not found" "waldo"; never_executed()]
  *)

  val pp : Format.formatter -> t -> unit
  (** Pretty print the response. *)
end

(** {2 Routing}

    Basic type-safe routing.
    @since 0.6 *)
module Route : sig
  type ('a, 'b) comp
  (** An atomic component of a path *)

  type ('a, 'b) t
  (** A route, composed of path components *)

  val int : (int -> 'a, 'a) comp
  (** Matches an integer. *)

  val string : (string -> 'a, 'a) comp
  (** Matches a string not containing ['/'] and binds it as is. *)

  val exact : string -> ('a, 'a) comp
  (** [exact "s"] matches ["s"] and nothing else. *)

  val return : ('a, 'a) t
  (** Matches the empty path. *)

  val rest : (string list -> 'a, 'a) t
  (** Matches a string, even containing ['/']. This will match
      the entirety of the remaining route.
      @since 0.7 *)

  val (@/) : ('a, 'b) comp -> ('b, 'c) t -> ('a, 'c) t
  (** [comp / route] matches ["foo/bar/…"] iff [comp] matches ["foo"],
      and [route] matches ["bar/…"]. *)

  val exact_path : string -> ('a,'b) t -> ('a,'b) t
  (** [exact_path "foo/bar/..." r] is equivalent to
      [exact "foo" @/ exact "bar" @/ ... @/ r]
      @since 0.11 **)

  val pp : Format.formatter -> _ t -> unit
  (** Print the route.
      @since 0.7 *)

  val to_string : _ t -> string
  (** Print the route.
      @since 0.7 *)
end

(** {2 Main Server type} *)

type t
(** A HTTP server. See {!create} for more details. *)

(** @inline *)
type listenning = Simple_httpd_domain.listenning =
  {
    addr : string;
    port : int;
    ssl  : Ssl.context option ;
  }
(** Type describing addresses we want to listen too, provided
    here to avoid module opening *)

val create :
  ?masksigpipe:bool ->
  ?max_connections:int ->
  ?num_thread:int ->
  ?timeout:float ->
  ?buf_size:int ->
  ?get_time_s:(unit -> float) ->
  ?listens:listenning list ->
  unit ->
  t
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

    @param get_time_s obtain the current timestamp in seconds.
      This parameter exists since 0.11.
*)

val listens : t -> Simple_httpd_domain.listenning list
(** Addresses and ports on which the server listens. *)

val status : t -> Simple_httpd_domain.status
(** Returns server status *)

val active_connections : t -> int
(** Number of active connections *)

(** {2 Filters} *)

(** Type of request filters. These filters may transform both the request and
    the response. Several method may share filter passed as optional parameters
    to function like {!add_route_handler}.

    The transformation of the response may depend on the request, Hence the
    type. For instance the filter provided by the optional module
    {!Simple_httpd_camlzip} uses this to compress the response only if
    [deflate] is allowed using the header named {!Headers.Accept_Encoding}. *)
type filter = byte_stream Request.t -> byte_stream Request.t *
                                         (Response.t -> Response.t)

val decode_request : (byte_stream -> byte_stream) -> (Headers.t -> Headers.t)
                     -> filter
(** helper to create a filter transforming only the request. *)

val encode_response : (Response.body -> Response.body) -> (Headers.t -> Headers.t)
                      -> filter
(** helper to create a filter transforming only the resposne. *)

val compose_embrace : filter -> filter -> filter
(** [compose_embrace f1 f2] compose two filters:
    the request will be passed first to [f2], then to [f1],
    the response will be passed first to [f2], then to [f1] **)

val compose_cross : filter -> filter -> filter
(** [compose_cross f1 f2] compose two filters:
    the request will be passed first to [f2], then to [f1],
    the response will be passed first to [f1], then to [f2] **)

(** {2 Route handlers}

    Here are the main function to explain what you server should to depending
    on the url send by the client.
*)

val add_route_handler :
  ?filter:filter ->
  ?meth:Meth.t ->
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
    - {!Route.exact s}, is the second, tried
    - {!Route.int}
    - {!Route.string}
    - {!Route.rest} is tried last.
    - In case of ambiguity, the first added route is tried first.

    @param meth if provided, only accept requests with the given method.
    Typically one could react to [`GET] or [`PUT].
    @param filter can be used to modify the request and response and also
    to reject some request using {!Response.fail_raise}. The default filter
    accept all requests and does not do any transformation.
*)

val add_route_handler_stream :
  ?filter:filter ->
  ?meth:Meth.t ->
  t ->
  ('a, byte_stream Request.t -> Response.t) Route.t -> 'a ->
  unit
(** Similar to {!add_route_handler}, but where the body of the request
    is a stream of bytes that has not been read yet.
    This is useful when one wants to stream the body directly into a parser,
    json decoder (such as [Jsonm]) or into a file. *)

(** {2 Server-sent events}

    {b EXPERIMENTAL}: this API is not stable yet. *)

(** A server-side function to generate of Server-sent events.

    See {{: https://html.spec.whatwg.org/multipage/server-sent-events.html} the w3c page}
    and {{: https://jvns.ca/blog/2021/01/12/day-36--server-sent-events-are-cool--and-a-fun-bug/}
    this blog post}.

    @since 0.9
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
  (** Close connection.
      @since 0.11 *)
end

type server_sent_generator = (module SERVER_SENT_GENERATOR)
(** Server-sent event generator *)

val add_route_server_sent_handler :
  ?filter:filter ->
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

    This handler stays on the original thread (it is synchronous).

    @since 0.9 *)

(** {2 Run the server} *)

val run : t -> (unit, exn) result
(** Run the main loop of the server, listening on a socket
    described at the server's creation time, using [new_thread] to
    start a thread for each new client.

    This returns [Ok ()] if the server exits gracefully, or [Error e] if
    it exits with an error. *)

(** {2 Debuggin/logging} *)

val debug : ?lvl:int ->
            ((('a, out_channel, unit, unit) format4 -> 'a) -> unit) -> unit
(** call [debug ~lvl (fun k -> k "format" args)] will output a debugging message
    on stdout. We currently use the following convention for levels:

    - [0]: normal unused
    - [1]: errors or very important events (the recommended production level)
    - [2]: give details of request or response
    - [>=3]: for debugging *)

val set_debug: int -> unit
(** Set the current debug level *)
