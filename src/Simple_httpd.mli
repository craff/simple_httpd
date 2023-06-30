(** {1 Static directory serving and page generation} *)

(** Some tools, like url encoding *)
module Util : sig
  (** {1 Some utils for writing web servers} *)

  val percent_encode : ?skip:(char -> bool) -> string -> string
  (** Encode the string into a valid path following
      https://tools.ietf.org/html/rfc3986#section-2.1
      @param skip if provided, allows to preserve some characters, e.g. '/' in a path.
   *)

  val percent_decode : string -> string
  (** Inverse operation of {!percent_encode}.
      Can raise [Invalid_argument "percent_decode"] if string is not valid
      percent encodings. *)

  val pp_date : Format.formatter -> Unix.tm -> unit
  (** Print date (given in GMT) in the expected format for http (for instance
      for expiration date of cookies. *)

  val split_query : string -> string * string
  (** Split a path between the path and the query. *)

  val split_on_slash : string -> string list
  (** Split a string on ['/'], remove the trailing ['/'] if any. *)

  val get_non_query_path : string -> string
  (** get the part of the path that is not the query parameters. *)

  val get_query : string -> string
  (** Obtain the query part of a path. *)

  val parse_query : string -> ((string*string) list, string) result
  (** Parse a query as a list of ['&'] or [';'] separated [key=value] pairs.
      The order might not be preserved.  *)
end

(** Module for declaring address and port to listen to *)
module Address : sig
  (** Module for declaring address and port to listen to *)

  (** A type to index all listened addresses *)
  type index = private int

  (** Record type storing an address we listen on *)
  type t =
    private
      { addr : string  (** The actual address in format "0.0.0.0" *)
      ; port : int     (** The port *)
      ; ssl  : Ssl.context option (** An optional ssl context *)
      ; reuse : bool   (** Can we reuse the socket *)
      ; mutable index : index (** The index used to refer to the address *)
      }

  (** The constructor to build an address *)
  val make : ?addr:string -> ?port:int -> ?ssl:Ssl.context ->
             ?reuse:bool -> unit -> t
end

(** Module dealing with the asynchronous treatment of clients by each domain *)
module Async : sig
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


  (** General functions related to OCaml's domain and asynchrone cooperative
    multithreading. *)

  (** Vocabulary:
      - socket: a file descriptor: may be the connection socket or another ressource
      like a connection to a database, a remote file etc.
      - client: a connection to the server. Each client has at least one socket (the
      connection socket)
      - session: an application can do several connections to the server and
      be identified as one session using session cookies.
   *)

  type session_data = ..
  type session_data += NoData

  (** Connection status. Holds the number of clients per domain.  *)
  type status = {
      nb_connections : int Atomic.t array
    }

  val string_status : status -> string

  (** Record describing clients *)
  type client

  val yield : unit -> unit
  (** let other threads run. Should be called for treatment that take time
      before sending results or reading data and when the other primitives
      can not be used. This happends typically for a pure computing task.
   *)

  val sleep : float -> unit
  (** Same as above, but with a minimum sleeping time in second *)

  val close : client -> unit
  (** Close the given client connection *)

  val flush : client -> unit
  (** Flushes clients output *)

  (** [schedule_io sock action] should be called when a non blocking read/write
      operation would have blocked. When read become possible, [action ()] will
      be called.  The return value should be (if possible) the number of bytes
      read or written. It this is meaningless, return a non zero value if some
      progress was made, while returning 0 will terminates the management of the
      client.

      A typical application for this is when interacting with a data base in non
      blocking mode. For just reading a socket, use the {!Io} module below.
   *)
  val schedule_io : Unix.file_descr -> (unit -> int) -> int
end

(** Module that encapsulates non blocking sockets with function similar to
    Unix.read and Unix.single_write but that will perform scheduling instead
    of blocking. This can be used to access your database. It has been tested
    with OCaml's bindings to [libpq].

    Io can be shared across several clients, for instance clients using the same
    session. As always, be aware of protecting concurrent access to the socket,
    as different client may run simultaneously on different domain.

    Io is completely useless on regular file or blocking sockets. *)
module Io : sig
    type t

    val create : Unix.file_descr -> t
    val close : t -> unit
    val read : t -> Bytes.t -> int -> int -> int
    val write : t -> Bytes.t -> int -> int -> int
  end

(** Representation of input streams, can be generated from string, file, ... *)
module Input : sig
  (** Input streams are used to represent a series of bytes that can arrive
    progressively.  For example, an uploaded file will be sent as a series of
    chunks. *)

  type t
  (** A buffered stream, with a view into the current buffer (or refill if empty),
      and a function to consume [n] bytes. *)

  val close : t -> unit
  (** Close stream *)

  val empty : t
  (** Stream with 0 bytes inside *)

  val make :
    ?bs:bytes ->
    ?close:(t -> unit) ->
    consume:(t -> int -> unit) ->
    fill:(t -> unit) ->
    unit -> t
  (** [make ~fill ()] creates a byte stream.
      @param fill is used to refill the buffer, and is called initially.
      @param close optional closing.
      @param init_size size of the buffer.
   *)

  val of_chan : ?buf_size:int -> in_channel -> t
  (** Make a buffered stream from the given channel. *)

  val of_fd : ?buf_size:int -> Unix.file_descr -> t
  (** Make a buffered stream from the given file descriptor. *)

  val of_client : ?buf_size:int -> Async.client -> t
  (** Make a buffered stream from the given http client's socket. *)

  val of_io : ?buf_size:int -> Io.t -> t
  (** Allow a to Make a buffered stream from the given {!Io.t}.
      The call will be scheduled if read blocks. *)

  val of_bytes : ?i:int -> ?len:int -> bytes -> t
  (** A stream that just returns the slice of bytes starting from [i]
      and of length [len]. *)

  val of_string : string -> t
  (** Make a buffered stream from the given string *)

  (** Module type for filling an input buffer by printing. A module of
      this type is openned in [<ML>] section of [.chamel] file when
      using [vfs_pack]. *)
  module type Output = sig
    val echo : string -> unit
    val printf : ('a, Format.formatter, unit, unit) format4 -> 'a
  end

  val of_output : ((module Output) -> unit) -> t
  (** Make a buffered stream from a function that will call the [echo] and
      [printf] function from the provided module. Using effect, the
      function producing the output is called lazily when it is needed
      to fill the buffer. *)

  val iter : (bytes -> int -> int -> unit) -> t -> unit
  (** Iterate on the chunks of the stream. *)

  val to_chan : out_channel -> t -> unit
  (** Write the stream to the channel. *)

  val with_file : ?buf_size:int -> string -> (t -> 'a) -> 'a
  (** Open a file with given name, and obtain an input stream
      on its content. When the function returns, the stream (and file) are closed. *)

  val read_char : t -> char

  val read_line : buf:Buffer.t -> t -> string
  (** Read a line from the stream.
      @param buf a buffer to (re)use. Its content will be cleared. *)

  val read_all : buf:Buffer.t -> t -> string
  (** Read the whole stream into a string.
      @param buf a buffer to (re)use. Its content will be cleared. *)

  val read_until : buf:Buffer.t -> target:string -> t -> unit
  (** Advance in the stream until in meet the given target.
      @param buf a buffer to (re)use. Its content will be cleared. *)

  val read_exactly :
    close_rec:bool -> size:int -> too_short:(int -> unit) ->
    t -> t
  (** [read_exactly ~size bs] returns a new stream that reads exactly
      [size] bytes from [bs], and then closes.
      @param close_rec if true, closing the resulting stream also closes
        [bs]
        @param too_short is called if [bs] closes with still [n] bytes remaining
   *)
end

(** Module providing logging facilities *)
module Log : sig
  (** Server Logging facility *)

  (** Currently there is a log level, you can set with this function.
      The default level is 1 and produces only errors (0 is totally silent).
      TODO: We plan a more explicit notion of logs *)
  val set_log_lvl : int -> unit

  (** With asynchronous communication, log can be mixed between domains.
      To address this issue, each domain will use a different file inside
      a provided log folder using [set_log_folder]. Logs contains the unix time
      and can be reordered if needed
      TODO: We plan to write tool to extract log information associated to
      a given client/session/request/... *)
  val set_log_folder : ?basename:string -> ?perm:int -> string -> int -> unit

  (** The log function. It must be used as
      [ Log.f ~lvl:n (fun k -> k fmt ...) ] using [ Printf ] format. *)
  val f : ?lvl:int ->
          ((('a, out_channel, unit, unit) format4 -> 'a) -> unit) -> unit
end

(** Module providing Mutex thats works with our [Async] module above *)
module Mutex : sig
  (** Simple_httpd notion of mutex. You must be careful with server wide mutex:
      a DoS attack could try to hold such a mutex. A mutex per session may be a good
      idea. A mutex per client is useless (client are treated sequentially).

      Note: they are implemented using Linux [eventfd] *)
  type t

  val create : unit -> t
  val try_lock : t -> bool
  val lock : t -> unit
  val unlock : t -> unit
end


(** Module defining HTML methods (GET,PUT,...) *)
module Method : sig
  (** {1 Methods}

      A short module defining the various HTTP methods (GET,PUT,...)*)

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
  val of_string : string -> t
end

(** Module to handle request and response headers *)
module Headers : sig
  (** {1 Headers}

    Headers are metadata associated with a request or response. This module provide
    the necessary function to read and modify headers *)

  (** A module defining all the legal header names, generated from the csv at
      {{:https://www.iana.org/assignments/http-fields/http-fields.xhtml} iana}.
   last update: 29.06.2023. Other headers are ignored! *)

  (** @inline *)
  include module type of Headers_

  type t = (header * string) list
  (** The header files of a request or response.

      Neither the key nor the value can contain ['\r'] or ['\n'].
      See https://tools.ietf.org/html/rfc7230#section-3.2 *)

  val empty : t
  (** Empty list of headers *)

  val get : ?f:(string->string) -> header -> t -> string option
  (** [get k headers] looks for the header field with key [k].
      @param f if provided, will transform the value before it is returned. *)

  val get_exn : ?f:(string->string) -> header -> t -> string
  (** [get_exn k headers] same as above but raise [Not_found] if the headers is
      not present. *)

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

(** Module to handle cookies *)
module Cookies : sig
  (** {1 Cookies}

    Cookies are data that are maintend both on server and clients.
    This is a module to get and set cookies in the headers. *)

  type t = (string * Http_cookie.t) list

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

(** Module handling HTML requests *)
module Request : sig
  (** {1 Requests}

      Requests are sent by a client, e.g. a web browser, curl or wget. *)

  type 'body t
  (** A request with method, path, host, headers, and a body, sent by a client.

      The body is polymorphic because the request goes through several
      transformations. First it has a body with a unread {!Simple_httpd.Input.t}
      stream, as only the request and headers are read; while the body might
      be entirely read as a string via {!read_body_full}.  *)

  val pp : Format.formatter -> string t -> unit
  (** Pretty print the request and its body *)

  val headers : _ t -> Headers.t
  (** List of headers of the request, including ["Host"] *)

  val get_header : ?f:(string->string) -> _ t -> Headers.header -> string option

  val get_header_int : _ t -> Headers.header -> int option

  val set_header : Headers.header -> string -> 'a t -> 'a t
  (** [set_header k v req] sets [k: v] in the request [req]'s headers. *)

  val update_headers : (Headers.t -> Headers.t) -> 'a t -> 'a t
  (** Modify headers *)

  val body : 'b t -> 'b
  (** Request body, possibly empty. *)

  val set_body : 'a -> _ t -> 'a t
  (** [set_body b req] returns a new query whose body is [b]. *)

  val cookies : _ t -> Cookies.t
  (** List of cookies of the request *)

  val get_cookie : _ t -> string -> Http_cookie.t option
  (** get a cookie *)

  val host : _ t -> string
  (** Host field of the request. It also appears in the headers. *)

  val meth : _ t -> Method.t
  (** Method for the request. *)

  val path : _ t -> string
  (** Request path. *)

  val client : _ t -> Async.client
  (** Request client *)

  val query : _ t -> (string*string) list
  (** Decode the query part of an url *)

  val start_time : _ t -> float
  (** time stamp (from [Unix.gettimeofday]) after parsing the first line of
    the request *)

  val trailer : _ t -> (Headers.t * Cookies.t) option
  (** trailer, read after a chunked body. Only maeningfull after the body stream
      is fully read and closed *)

  val close_after_req : _ t -> bool
  (** Tells if we are supposed to close the connection after answering the request *)

  val read_body_full : buf:Buffer.t -> Input.t t -> string t
  (** Read the whole body into a string. *)
end

(** Module defining HTML response codes *)
module Response_code : sig
  (** {1 Response Codes}

    Response code allows client to know if a request failed and give a reason.
    generated from csv at {{:https://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml#http-status-codes} iana} *)

  (** @inline *)
  include module type of Response_code
end

(** Module handling HTML responses *)
module Response : sig
  (** {1 Responses}

      Responses are what a http server, such as {!Simple_httpd}, send back to
      the client to answer a {!Request.t}*)

  type body = String of string
            | Stream of Input.t
            | File of
                { fd : Unix.file_descr
                ; size : int
                ; close : bool
                  (** if using sendfile, one might want to maintain the fd open
                      for another request, sharing file descriptor would limit
                      the number of open files *)}
            | Void
  (** Body of a response, either as a simple string, or a stream of bytes, a
      file or nothing (for instance for server-sent events). *)

  type t
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
    Input.t ->
  t
  (** Same as {!make_raw} but with a stream body. The body will be sent with
      the chunked transfer-encoding. *)

  val make_raw_file :
    ?cookies:Cookies.t ->
    ?headers:Headers.t ->
    code:Response_code.t ->
    close:bool ->
    int -> Unix.file_descr ->
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
    ?cookies:Cookies.t ->
    ?headers:Headers.t ->
    Input.t -> t
  (** Same as {!make} but with a stream body. *)

  val make_file :
    ?cookies:Cookies.t ->
    ?headers:Headers.t -> close:bool ->
    int -> Unix.file_descr -> t
  (** Same as {!make} but with a file_descr body. *)

  val fail :
    ?cookies:Cookies.t ->
    ?headers:Headers.t -> code:Response_code.t ->
    ('a, unit, string, t) format4 -> 'a
  (** Make the current request fail with the given code and message.
      Example: [fail ~code:404 "oh noes, %s not found" "waldo"].
   *)

  val fail_raise : ?headers:Headers.t -> ?cookies:Cookies.t -> code:Response_code.t
                   -> ('a, unit, string, 'b) format4 -> 'a
  (** Similar to {!fail} but raises an exception that exits the current handler.
      This should not be used outside of a (path) handler.
      Example: [fail_raise ~code:not_found "oh noes, %s not found" "waldo"; never_executed()]
   *)

  val pp : Format.formatter -> t -> unit
  (** Pretty print the response. *)
end

(** Module defining Routes to select which function will answer a request *)
module Route : sig
  (** {1 Routing}

      Basic type-safe routing. *)

  type ('a, 'b) comp
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
      the entirety of the remaining route. *)

  val (@/) : ('a, 'b) comp -> ('b, 'c) t -> ('a, 'c) t
  (** [comp / route] matches ["foo/bar/…"] iff [comp] matches ["foo"],
      and [route] matches ["bar/…"]. *)

  val exact_path : string -> ('a,'b) t -> ('a,'b) t
  (** [exact_path "foo/bar/..." r] is equivalent to
      [exact "foo" @/ exact "bar" @/ ... @/ r] **)

  val pp : Format.formatter -> _ t -> unit
  (** Print the route. 0.7 *)

  val to_string : _ t -> string
  (** Print the route. 0.7 *)

  (** {1 Filters} *)

  (** Type of request filters. These filters may transform both the request and
      the response. Several method may share filter passed as optional parameters
      to function like {!Server.add_route_handler}.

      The transformation of the response may depend on the request, Hence the
      type. For instance the filter provided by the optional module
      {{:../../simple_httpd_caml*zip/Simple_httpd_camlzip/index.html}Simple_httpd_camlzip} uses this to compress the
      response only if [deflate] is allowed using the header named
    {!Headers.Accept_Encoding}. *)
end

module Filter : sig
  type 'a t = 'a Request.t -> 'a Request.t * (Response.t -> Response.t)

  val decode_request : ('a -> 'a) -> (Headers.t -> Headers.t)
                       -> 'a t
  (** helper to create a filter transforming only the request. *)

  val encode_response : (Response.body -> Response.body) -> (Headers.t -> Headers.t)
                        -> 'a t
  (** helper to create a filter transforming only the resposne. *)

  val compose_embrace : 'a t -> 'a t -> 'a t
  (** [compose_embrace f1 f2] compose two filters:
      the request will be passed first to [f2], then to [f1],
      the response will be passed first to [f2], then to [f1] **)

  val compose_cross : 'a t -> 'a t -> 'a t
  (** [compose_cross f1 f2] compose two filters:
      the request will be passed first to [f2], then to [f1],
      the response will be passed first to [f1], then to [f2] **)
end

module Camlzip : sig
  val filter :
    ?compress_above:int ->
    ?buf_size:int -> unit -> Input.t Filter.t
  (** Filter responsible for deflate compression/decompression. *)

  val deflate_string : ?buf_size:int -> string -> string
  (** zlib string compression *)

  val accept_deflate : 'a Request.t -> bool
  (** check if request accept deplate *)

  val file_deflate : string -> string -> unit
  (** [file_deflate file1 file2] Compress file1 into file2. file2 is erased *)

end

(** provide a filter giving very simple statistics. We can do much better
    but be carefull on how to do it *)
module Stats : sig

(** This a filter to acquire statistics.
    [let (filter, get) = Stats.filter ()]
    will give you a [Filter.t] and a function [get] returning the statistics
    as a string
    ["N requests (average response time: Tms = T1ms (read) + T2ms (build))"]
 *)
val filter : unit -> 'a Filter.t * (unit -> string)

(** Note: currently we can not measure the time to write the response. *)
end

(** Module to handle session data *)
module Session : sig
  (** This module allows to mange session which are common to several client
      and can survive a deconnection of the clients. This does not provide
      any form of authentication, but it is easy to use them to implement
      authentication. *)

  type session
  (** type for session *)

  type session_data = Async.session_data
  (** This type is an extensible variant that you can extend to hold some data
      which resides in the server memory.  These data will be lost if the
      server reboots.

      Possible uses are
      - data that you do not want to be on the client but that do not need
        to persist server reboot, like a session private key.
      - {!Io.t} sockets shared among multiple clients, for instance for a
        database connection
      - {!Mutex.t} to protect the above.
   *)

  val check : ?session_life_time:float ->
              ?init:(unit -> session_data) ->
              ?finalise:(session_data -> unit) ->
              ?check:(session -> bool) ->
              ?error:(Response_code.t*Headers.t) ->
              'a Filter.t
  (** Check or create a new session. This is a filter!

      @param session_life_time: session are destroyed if not accessed after
        this time.
      @param initial value for the session data (default: NoData)
      @param finalise function called on the session data when it is detroyed.
      @param check some extra check, the session will be destroy if it fails.
      @param error status code and hadears to send in case of error. Can
        be used to redirect to a login or error page *)

  (** get the client session. The session must have been initialized with [check]
      otherwise NoSession may be raised *)
  val get_session : 'a Request.t -> session
  exception NoSession

  (** get the session data from a session *)
  val get_session_data : session -> session_data

  (** update the session data *)
  val set_session_data : session -> session_data -> unit

  (** update the session data and compute a new value at once *)
  val do_session_data :
    session -> (session_data -> 'a * session_data) -> 'a

  (** get a session cookie *)
  val get_session_cookie : session -> string -> string option

  (** set a session cookie *)
  val set_session_cookie : session -> string -> string -> unit

  (** remove all server side session information *)
  val delete_session : session -> unit

(** TODO: different life time if there is live client attached to the
    session. *)
end

(** Some HTML combinators *)
module Html : sig
  (** HTML combinators.

      This module provides combinators to produce html. It doesn't enforce
      the well-formedness of the html, unlike Tyxml, but it's simple and should
      be reasonably efficient.
   *)

  (** @inline *)
  include module type of Html_

  (** Convert a HTML element to a string.
    @param top if true, add DOCTYPE at the beginning. The top element should then
    be a "html" tag. *)
  val to_string : ?top:bool -> elt -> string

  (** Convert a list of HTML elements to a string.
    This is designed for fragments of HTML that are to be injected inside
    a bigger context, as it's invalid to have multiple elements at the toplevel
    of a HTML document. *)
  val to_string_l : elt list -> string

  val to_stream : elt -> Input.t
end

(** Module to create a server and run it *)
module Server : sig
  (** {1 Main Server type} *)

  type t
  (** A HTTP server. See {!create} for more details. *)

  module type Parameters = sig
    val max_connections : int ref
    val num_threads : int ref
    val timeout : float ref
    val buf_size : int ref
    val ktls : bool ref

    val log_lvl : int ref
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
    ?filter:Input.t Filter.t ->
    t ->
    ('a, string Request.t -> Response.t) Route.t -> 'a ->
    unit
  (** [add_route_handler server route f] add a route to give a [string] as
      response.

      For instance, [add_route_handler serverRoute.(exact "path" @/ string @/
      int @/ return) f] calls [f "foo" 42 request] when a [request] with path
      "path/foo/42/" is received.

      Note that the handlers are called in the following precision order:
      - {!Route.return}, accepting only the empty url is the most precise
      - [{!Route.exact} s], is the second, tried
      - {!Route.int}
      - {!Route.string}
      - {!Route.rest} is tried last.
      - In case of ambiguity, the first added route is tried first.

      @param adresses if provided, only accept requests from the given
        address and port. Will raise
        [Invalid_argument "add_route: the server is not listening to that adress"]
        if the server is not listenning to that adresse and port.
      @param hostnames if provided, only accept requests from the given
        hosts as seen in the [Host] header field. No dns check is performed.
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
    ?filter:Input.t Filter.t ->
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
    ?filter:Input.t Filter.t ->
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

end

(** Module to server directory structure (real of virtual) *)
module Dir : sig
  (** Serving static content from directories

    This module provides the same functionality as the "http_of_dir" tool.
    It exposes a directory (and its subdirectories), with the optional ability
    to delete or upload files. *)

  (** behavior of static directory.

      This controls what happens when the user requests the path to
      a directory rather than a file. *)
  type dir_behavior =
    | Index
    (** Redirect to index.html if present, else fails. *)
    | Lists
    (** Lists content of directory. Be careful of security implications. *)
    | Index_or_lists
    (** Redirect to index.html if present and lists content otherwise.
        This is useful for tilde ("~") directories and other per-user behavior,
        but be mindful of security implications *)
    | Forbidden
  (** Forbid access to directory. This is suited for serving assets, for example. *)

  (** configuration for static file handlers. This might get
      more fields over time. *)
  type config = {
      mutable download: bool;
      (** Is downloading files allowed? *)

      mutable dir_behavior: dir_behavior;
      (** Behavior when serving a directory and not a file *)

      mutable delete: bool;
      (** Is deleting a file allowed? (with method DELETE) *)

      mutable upload: bool;
      (** Is uploading a file allowed? (with method PUT) *)

      mutable max_upload_size: int;
      (** If {!upload} is true, this is the maximum size in bytes for
          uploaded files. *)
    }

  (** default configuration: [
      { download=true
      ; dir_behavior=Forbidden
      ; delete=false
      ; upload=false
      ; max_upload_size = 10 * 1024 * 1024
      }] *)
  val default_config : unit -> config

  val config :
    ?download:bool ->
    ?dir_behavior:dir_behavior ->
    ?delete:bool ->
    ?upload:bool ->
    ?max_upload_size:int ->
    unit ->
    config
  (** Build a config from {!default_config}. *)

  val add_dir_path :
    ?addresses: Address.t list ->
    ?hostnames: string list ->
    ?filter:Input.t Filter.t ->
    ?prefix:string ->
    ?config:config ->
    dir:string ->
    Server.t -> unit
  (** [add_dirpath ~config ~dir ~prefix server] adds route handle to the
      [server] to serve static files in [dir] when url starts with [prefix],
      using the given configuration [config]. Method is always [GET].
    @param addresses like for {!Server.add_route_handler}
    @param hostnames like for {!Server.add_route_handler}
    @param filter like for {!Server.add_route_handler}
   *)

  (** {2 Virtual file system.}

      This is used to emulate a file system from pure OCaml functions and
      data, e.g. for resources bundled (totally of partially) inside the web
      server memory. *)

  (** {!VFS} allows to serve dynamic content produced using such a record *)
  type dynamic =
    { input : string Request.t -> Input.t
      (** The answer body will be produced by this function *)
    ; filter : 'a. 'a Filter.t option
      (** dynamic content may contain an optional filter. If the
          route has other filter, this filter with process the request last
          and process the input stream first. *)
    }

  (** Here is the type of content that can be served by a {!VFS} *)
  type 'a content =
    | String of string * string option
    (** [String(s,Some sz)] is a fixed string content. [s] is the content,
        [sz], if provided is a compressed version of [s] produced by {!Camlzip}. *)
    | Path   of string * (string * int) option
    (** [Path(fname,Some(fzname,size))] is a real file [fname], possibly with a
        compressed version [fzname] of the given [size] *)
    | Dynamic of dynamic
    (** Dynamic content *)
    | Stream of Input.t
    (** Streamed content *)
    | Fd of Unix.file_descr
    (** Content with a file descriptor, that will be send using sendfile. *)
    | Dir of 'a
    (** Used internally *)

  (** Description of a file in a {!VFS} *)
  type file_info =
    FI : { content : 'a content (** content *)
         ; size : int option (** size (must be accurate, if provided, will use
                                 chunked encoding if not provided. *)
         ; mtime : float option (** modification time for caching *)
         ; headers : Headers.t (** extra headers *)
         } -> file_info

  module type VFS = sig
    val descr : string
    (** Description of the VFS *)

    val is_directory : string -> bool

    val contains : string -> bool
    (** [file_exists vfs path] returns [true] if [path] points to a file
        or directory inside [vfs]. *)

    val list_dir : string -> string array
    (** List directory. This only returns basenames, the files need
        to be put in the directory path using [Filename.concat]. *)

    val delete : string -> unit
    (** Delete path *)

    val create : string -> (bytes -> int -> int -> unit) * (unit -> unit)
    (** Create a file and obtain a pair [write, close] *)

    val read_file : string -> file_info
    (** Read content of a file *)

  end

  val vfs_of_dir : string -> (module VFS)
  (** [vfs_of_dir dir] makes a virtual file system that reads from the
      disk.
   *)

  val add_vfs :
    ?addresses: Address.t list ->
    ?hostnames: string list ->
    ?filter:Input.t Filter.t ->
    ?prefix:string ->
    ?config:config ->
    vfs:(module VFS) ->
    Server.t -> unit
  (** Similar to {!add_dir_path} but using a virtual file system instead.
   *)

  (** An embedded file system, as a list of files with (relative) paths.
      This is useful in combination with the "simple-httpd-mkfs" tool,
      which embeds the files it's given into a OCaml module.
   *)
  module Embedded_fs : sig
    type t
    (** The pseudo-filesystem *)

    val create : ?top:string -> ?mtime:float -> unit -> t
    (** create a new pseudo file system. *)

    val add_file : t -> path:string -> ?mtime:float -> ?headers:Headers.t ->
                   string -> unit
    (** [add_file ~path content] add at [path] with the given [content] to the
        virtual file system.
        @raise Invalid_argument if the path contains '..' or if it tries to
          make a directory out of an existing path that is a file.

        @param path: the name of the file (in the url)
        @param mtime: that last modification time, used for caching.
        @param headers: extra headers, like mime type.
     *)

    val add_dynamic : t -> path:string ->
                      ?mtime: float -> ?headers:Headers.t -> dynamic -> unit
    (** Add some dynamic content to the virtual file system.
        @param path: the name of the file (in the url)
        @param mtime: that last modification time, used for caching.
        @param headers: extra headers, like mime type.
     *)

    val add_path : t -> path:string ->
                   ?mtime:float -> ?headers:Headers.t ->
                   ?deflate:string -> string -> unit
    (** [add_path vfs ~path real_path] add a [path] to a file on disk at [real_path]
        in the embedded file system
        @param deflate: a path on the disk to a compressed version of the
          file with {!Camlzip}.
        @param path: the name of the file (in the url)
        @param mtime: that last modification time, used for caching.
        @param headers: extra headers, like mime type.
     *)

    val to_vfs : t -> (module VFS)
    (** Convert a pseudo file system into a vfs to use with {!Dir.add_vfs}. *)

  end
end

(** Hight level module to write server handling multiple hosts/addresses *)
module Host : sig
  open Server
  open Dir

  (** A module of this type is provided to Init functor to allow you to
      initialize the route of your host *)
  module type HostInit = sig
    val server : t

    val add_route_handler :
      ?meth:Method.t ->
      ?filter:Input.t Filter.t ->
      ('a, string Request.t -> Response.t) Route.t -> 'a -> unit
    (** Same as {!Server.add_route_handler} but the server is provided when
        the host is attached to it by {!start_server}. *)

    val add_route_handler_stream :
      ?meth:Method.t ->
      ?filter:Input.t Filter.t ->
      ('a, Input.t Request.t -> Response.t) Route.t -> 'a -> unit
    (** Same as {!Server.add_route_handler_stream} but the server is provided when
        the host is attached to it by {!start_server}. *)

    val add_dir_path :
      ?filter:Input.t Filter.t ->
      ?prefix:string ->
      ?config:config ->
      string -> unit
    (** Same as {!Dir.add_dir_path} but the server is provided when
        the host is attached to it by {!start_server}. *)

    val add_vfs :
      ?filter:Input.t Filter.t ->
      ?prefix:string ->
      ?config:config ->
      (module VFS) -> unit
    (** Same as {!Dir.add_vfs} but the server is provided when
        the host is attached to it by {!start_server}. *)
  end

  (** Define a module a this type to serve some "host" *)
  module type Host = sig
    val addresses : Address.t list
    (** accept only request from this addresses (or any address if the list is
        empty *)

    val hostnames : string list
    (** accept only request from this hostnames (or any hostname if the list is
        empty *)

    module Init(_:HostInit) : sig end
    (** This function must initialize all the route of your server.*)
  end

  val start_server : (module Server.Parameters) -> (module Host) list -> unit
  (** start a server with the given list of Host *)
end
