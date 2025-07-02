(** {1 Static directory serving and page generation} *)

(** Some tools, like url encoding *)
module Util : sig
  (** {1 Some utils for writing web servers} *)

  val percent_encode : ?skip:(char -> bool) -> string -> string
  (** [percent_encode ~skip s] Encodes the string [s] into a valid path
      following {{:https://tools.ietf.org/html/rfc3986#section-2.1}rfc 3986
      section 2.1}
      @param skip if provided, allows to preserve some characters, e.g. '/' in a path.
   *)

  val percent_decode : string -> string
  (** Inverse operation of {!percent_encode}.
      Can raise [Invalid_argument "percent_decode"] if string is not valid
      percent encodings. *)

  val pp_date : Unix.tm -> string
  (** Print date (given in GMT) in the expected format for http (for instance
      for expiration date of cookies. *)

  val date_of_epoch : float -> string
  (** Convert epoch to http date *)

  val date_to_epoch : string -> float
  (** Convert date to epoch *)

  val split_query : string -> string * string
  (** Split a path between the path and the query. *)

  val split_on_slash : string -> string list
  (** Split a string on ['/'], remove the trailing ['/'] if any. *)

  val get_non_query_path : string -> string
  (** get the part of the path that is not the query parameters. *)

  val get_query : string -> string
  (** Obtain the query part of a path. *)

  exception Invalid_query of string
  val parse_query : string -> (string*string) list
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
      ; hosts: string list (** The host we accept: any if the list is empty,
                               only those listed otherwise *)
      ; ssl  : Ssl.context option (** An optional ssl context *)
      ; reuse : bool   (** Can we reuse the socket *)
      ; mutable index : index ref (** The index used to refer to the address,
                                      shared by all addresses with the same IP
                                      and port*)
      }

  (** type givent the relevant information for a ssl certificate *)
  type ssl_info =
    { protocol : Ssl.protocol (** minimum protocol to use, max is TSL1.3 *)
    ; cert : string           (** file name of the certificate *)
    ; priv : string           (** file name of the private key *)
    }

  (** The constructor to build an address *)
  val make : ?addr:string -> ?port:int -> ?hosts:string list -> ?ssl:ssl_info ->
             ?reuse:bool -> unit -> t

  (** Functions to reuse the same address and ssl certificate with a different
      port or hosts. *)
  val change_hosts : string list -> t -> t
  val change_port  : int -> t -> t

  (** set the period in seconds at which all ssl certificates are checked for
      renewal (default 1 day) *)
  val set_ssl_reload_period : int -> unit

end

(** Module allowing to retrive information about clients *)
module Client : sig
  type t

  (** Is the client connected *)
  val connected : t -> bool

  (** Ip address of the peer *)
  val peer : t -> string

  (** Unix time of the client arrival*)
  val start_time : t -> float

  (** Is the client using ssl *)
  val is_ssl : t -> bool

  (** close the client connection *)
  val close : t -> unit

  (** flush an the ssl socket of the client if it has one *)
  val ssl_flush : t -> unit
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

  val yield : unit -> unit
  (** let other threads run. Should be called for treatment that take time
      before sending results or reading data and when the other primitives
      can not be used. This happends typically for a pure computing task.
   *)

  val sleep : float -> unit
  (** Same as above, but with a minimum sleeping time in second *)

  (** Run the given function concurrently. Beware that this is cooperative
      threading. Typical use is a data base request that write to one end
      of a fifo, while the web server reads the other end. *)
  val spawn : (unit -> unit) -> unit

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

    val create : ?edge_triggered:bool -> ?finalise:(t -> unit) -> Unix.file_descr -> t
    (** Encapsulate a file descriptor in a dedicated data structure.
        finalise default to Unix.close Io.sock. *)

    val close : t -> unit
    (** Close io, similar to [Unix.close], simply call finalise*)

    val read : t -> Bytes.t -> int -> int -> int
    (** Read, similar to [Unix.read], but asynchronous *)

    val write : t -> Bytes.t -> int -> int -> int
    (** Read, similar to [Unix.write], but asynchronous *)

    val sock : t -> Unix.file_descr
    (** Returns the socket, usefull in finalise or to set some socket
        properties, do not attempt to read/write the socket. *)

    val formatter : t -> Format.formatter
    (** Provide a formatter to use with the [Format] library *)

    val schedule: t -> unit
    (** Allow to schedule the io immediatly. Typical use is opening a pipe and
        waiting the other end to be ready. *)

    val poll : ?edge_trigger:bool ->
               ?read:bool ->
               ?write:bool ->
               Unix.file_descr -> unit
    (** Low level polling, release control until the event is available.
        Usefull for Postgresql. Beware that libpq and other do not support
        Edge Trigerring, which is true by defaut. read and write are false
        by default. *)
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

  val of_client : ?buf_size:int -> Client.t -> t
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

  val end_of_input : t -> bool
  (** returns true if we are at the end of the input (self.len = 0 and fill returns 0) *)

  val eof : char
  (** the chracter '\255'*)

  val read_char : t -> char
  (** read one char, returns {!Input.eof} u=if the input buffer is empty *)

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

  (** The following are a minimal set of non backtracking parsing combinators
      with no allocation. They use {!Input.eof} when the stream is empty*)
  exception FailParse of int
  val fail_parse : t -> 'a
  (** fail, return the current offset *)

  val branch_char : (char -> t -> 'a) -> t -> 'a
  (** branch from the value of one char *)

  val exact_char : char -> 'a -> t -> 'a
  (** parse exactly one char *)

  val exact_string : string -> 'a -> t -> 'a
  (** parse exactly the given string *)

  val star : (t -> unit) -> t -> unit
  (** repeat parsing 0 or more time, fail if partial parsing of one more item
      is possible *)

  val plus : (t -> unit) -> t -> unit
  (** repeat parsing 1 or more time, fail if partial parsing of one more item
      is possible *)

  val blank : t -> unit
  (** parse blank charaters *)

  val space : t -> unit
  (** parse spaces *)

  val int : t -> int
  (** parse a positive integer in decimal *)

  val current : t -> string
  (** return the current value of the buffer, usefull when capturing
      [Fail_parse n] *)
end

(** Module providing logging facilities *)
module Log : sig
  (** Server Logging facility *)

  (** There are 3 kinds of log with separate log level *)
  type log_lvl =
    | Req of int (** log request: at level 1 => information to log
                     all request. > 1: more detail on the treatment of the request *)
    | Sch of int (** log the scheduler: mainly for debugging *)
    | Exc of int (** Exception: 1 => only error that are important *)
    | Aut of int (** Authentications: 1 => all login/logout *)
    | Prc of int (** Processes: 1: start and stop, 2: details of waitpid *)
    | Usr of int (** User specific log *)

  (** Set log level for requests. [set_log_request n] will show all log called
      with [Req p] when p < n. *)
  val set_log_requests : int -> unit

  (** same as above for the scheduler *)
  val set_log_scheduler : int -> unit

  (** same as above for exceptions *)
  val set_log_exceptions : int -> unit

  (** same as above for authentications *)
  val set_log_authentications : int -> unit

  (** same as above for processes *)
  val set_log_processes       : int -> unit

  (** same as above for user specific logs *)
  val set_log_user             : int -> unit

  (** With asynchronous communication, log can be mixed between domains.
      To address this issue, each domain will use a different file inside
      a provided log folder using [set_log_folder]. Logs contains the unix time
      and can be reordered if needed
      TODO: We plan to write tool to extract log information associated to
      a given client/session/request/... *)
  val set_log_folder : ?basename:string -> ?perm:int -> string -> int -> unit

  (** The log function. It must be used as
      [ Log.f ~lvl:n (fun k -> k fmt ...) ] using [ Printf ] format. *)
  val f : log_lvl ->
          ((('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) -> unit
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
  val delete : t -> unit
end

module Semaphore : sig
  type t
  val create : int -> t
  val decr : Mutex.t -> t -> unit
  val try_decr : t -> bool
  val incr : t -> unit
  val delete : t -> unit
end

module Resources : sig
  module type Resource = sig
    type t
    val create : unit -> t
    val number : int
  end

  module type Resources = sig
    type t
    type handle
    val get : unit -> handle * t
    val release : handle -> unit
    val use : (t -> 'a) -> 'a
  end

  module Make(R:Resource) : Resources with type t = R.t
end

(** Module for creating process to communicate with.
   reading and writing are non blocking. *)
module Process : sig
  (** type describing processed status *)
  type status = Running | Exited of int | Exn of exn

  (** record holding process information *)
  type process = private
    { pid : int
    ; mutable status : status }

  (** [create_process ~wait_interval ~stdout ~stderr cmd args] creates a new
      process, with the given command name and args. It returns a unix domain
      socket of type [Io.t] connected to the standard input and output of the
      process, unless you provide values for stderr and stdout.

      When the Io.t channel is closed, the process is waited for using
      [Unix.waitpid [WNOHANG; WUNTRACED]], if a loop that yield CPU for
      wait_interval second (default 0.010s = 10ms). *)
  val create : ?wait_interval: float ->
               ?stdout: Unix.file_descr -> ?stderr: Unix.file_descr ->
               string -> string array -> process * Io.t
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

  (** A module defining all the legal header names, generated from the
      {{:https://www.iana.org/assignments/http-fields/field-names.csv}csv at iana}.
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

  type t = Http_cookie.t list

  val empty : t
  val parse : string -> t
  val add : Http_cookie.t -> t -> t
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

  val get_cookie_string : _ t -> string -> string
  (** get a cookie value, may raise [Not_found] *)

  val get_cookie_int : _ t -> string -> int
  (** get a cookie value as int, may raise [Not_found] or [Failure "int_of_string"] *)

  val set_cookie : 'a t -> Http_cookie.t -> 'a t
  (** set a cookie. Note: this could be useful in a filter.

      Beware that cookie in the request are not automatically refreshed and
      added to the response.  Your handler must read the request cookies and
      send the new_cookies using the optional parameters [~cookies] of the
      function in the {!Response} module.

      Dynamic response provided by the {!Dir} module also offers the possibility
      to set the cookies.
   *)

  val host : _ t -> string
  (** Host field of the request. It also appears in the headers. *)

  val meth : _ t -> Method.t
  (** Method for the request. *)

  val path : _ t -> string
  (** Request path (including query). *)

  val path_components : _ t -> string list
  (** Request path components without query *)

  val client : _ t -> Client.t
  (** Request client *)

  val query : _ t -> (string*string) list
  (** Decode the query part of an url *)

  val multipart_headers : _ t -> ((string*Headers.header)*string) list
  (** Contains the headers included for each multipart section in the body.
    Filename is included as a fake header Headers.Filename_Multipart *)

  val start_time : _ t -> float
  (** time stamp (from [Unix.gettimeofday]) after parsing the first line of
    the request *)

  val reset_timeout : _ t -> unit
  (** reset the timeout for a request if it needs time *)

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

module Sfd : sig
  type t
  val make : Unix.file_descr -> t
  val close : t -> unit
  val get  : t -> Unix.file_descr
end

(** Module handling HTML responses *)
module Response : sig
  (** {1 Responses}

      Responses are what a http server, such as {!Simple_httpd}, send back to
      the client to answer a {!Request.t}*)

  type body = String of string
            | Stream of
                { inp : Input.t
                ; synch : (unit -> unit) option
                (** flush each part and call f if synch is [Some f] *)
                ; size : int option
                (** use chunk encoding only if size is not provided *)
                ; close : Input.t -> unit
                (** close stream with this function at end of input *)}
            | File of
                { fd : Sfd.t
                ; size : int
                ; close : Sfd.t -> unit
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

  val set_post : (unit -> unit) -> t -> t
  (** Set the post function, run after sending the response *)

  val get_post : t -> (unit -> unit)
  (** Get the post function, run after sending the response, typically to compse it
      with a new one. *)

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
    ?close:(Sfd.t -> unit) ->
    code:Response_code.t ->
    int -> Sfd.t ->
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
    ?synch:(unit -> unit) ->
    ?close:(Input.t->unit) ->
    ?cookies:Cookies.t ->
    ?headers:Headers.t ->
    ?size:int ->
    Input.t -> t
  (** Same as {!make} but with a stream body. *)

  val make_file :
    ?cookies:Cookies.t ->
    ?headers:Headers.t ->
    ?close:(Sfd.t -> unit) ->
    int -> Sfd.t -> t
  (** Same as {!make} but with a file_descr body. *)

  val fail :
    ?cookies:Cookies.t ->
    ?headers:Headers.t ->
    code:Response_code.t ->
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

  val pass : unit -> 'a
  (** This function may be called in a handler to try the next request.
      It must be raised after reading only the path, not the request.
      Your handler should look like:
      [fun path -> if bad path then pass (); fun req -> ...]
   *)
end

(** {1 Filters} *)

(** Type of request filters. These filters may transform both the request and
    the response. Several method may share filter passed as optional parameters
    to function like {!Server.add_route_handler}.

    The transformation of the response may depend on the request, Hence the
    type. For instance the filter provided by the optional module
    {{:../../simple_httpd_caml*zip/Simple_httpd_camlzip/index.html}Simple_httpd_camlzip}
    uses this to compress the response only if [deflate] is allowed using the
    header named {!Headers.Accept_Encoding}. *)
module Filter : sig
  type 'a t = 'a Request.t -> 'a Request.t * (Response.t -> Response.t)

  val decode_request : ('a -> 'a) -> (Headers.t -> Headers.t)
                       -> 'a t
  (** helper to create a filter transforming only the request. *)

  val encode_response : (Response.body -> Response.body) -> (Headers.t -> Headers.t)
                        -> 'a t
  (** helper to create a filter transforming only the resposne. *)

  val idt : 'a t
  (** identity filter *)

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

(** Module to handle session data *)
module Session : sig
  (** This module allows to manage sessions which are common to several clients
      and can survive a deconnection of the clients. This does not provide
      any form of authentication, but it is easy to use them to implement
      authentication.

      It is recommended to create session only for authenticated connection:
      sessions require some memory (around 100 bytes) on the server, and if
      you have a session life time of one hour, an attaquant could exhaust
      available memory. Unauthenticated session with a short life comparable
      to the connection timeout could be acceseptable, as clients also uses
      some memory. *)

  type t
  (** type for session *)

  type data
  type 'a key
      (** session can hold several values of arbitrary type associated to a
          key The type of key is an extensible variant that you can extend to
          hold some data which resides in the server memory.  These data will
          be lost if the server reboots.

      Possible uses are
      - data that you do not want to be on the client but that do not need
        to persist server reboot, like a session private key.
      - {!Io.t} sockets shared among multiple clients, for instance for a
        database connection
      - {!Mutex.t} to protect the above.
   *)

  (** [new_key ()] creates a new key, to associate data.

      The optional argument [cleanup_delete] is called when the session is
      deleted.

      The optional argument [cleanup_no_client] is clalled when no more client
      are connected to that session or when the session is deleted. If it
      returns [false], the data is deleted, otherwise, it is kept.

      [cleanup_delete] is called if and only if [cleanup_no_client] returns
      [true].  *)
  val new_key : ?cleanup_delete:('a -> unit) ->
                ?cleanup_no_client:('a -> bool) ->
                ?save:(out_channel -> 'a -> unit) ->
                ?load:(in_channel -> 'a) ->
                string -> 'a key

  (** element of this type control the managment of cookies *)
  type cookie_policy =
    { path : string (** cookies path *)
    ; base : string (** cookies base name, the cookie's name will be
                        "__Host-"^basename^suffix *)
    ; life : float  (** session life time in seconds *)
    ; filter : Http_cookie.t -> Http_cookie.t option
      (** tells which cookies should be deleted together with the session key
          and address *)}

  (**   { path = "/"
        ; base = "Session"
        ; filter = fun _ -> None } *)
  val default_cookie_policy : cookie_policy

  val start_check: ?create:bool ->
            ?check:(t -> bool) ->
            ?cookie_policy:cookie_policy ->
            ?error:(Response_code.t*Headers.t) ->
            'a Request.t -> Cookies.t * t
   (** Check or create a new session and add the session cookies to the
       cookie of the request. This can fail if
       - the session cookie is changed
       - the ip address changes
       - the function check returns false

       If it fails, all cookies in the request are expired by the resposne sent.

      @param initial value for the session data (default: NoData)
      @param finalise function called on the session data when it is detroyed.
      @param create if false no session is created if there was none
      @param check some extra check, the session will be destroy if it fails.
      @param filter this parameter is called on all request cookies.  If the
        filter return None, the cookie is expired. The default is to keep all
        cookies unchanged. The session cookies named ["SESSION_KEY"] and
        ["SESSION_ADDR"] are not passed to the filter.
      @param error status code and hadears to send in case of error. Can
        be used to redirect to a login or error page. *)

  val filter :
            ?check:(t -> bool) ->
            ?cookie_policy:cookie_policy ->
            ?error:(Response_code.t*Headers.t) ->
            'a Filter.t
  (** Same as above as a filter. The cookies are added to the response. *)

  (** get the client session is any. No check is performed on the session cookie *)
  val get_session : ?cookie_policy:cookie_policy -> 'a Request.t -> t option

  (** get the session data associated to the given key from a session.
      raises [Not_found] if the key is not present *)
  val get_session_data : t -> 'a key -> 'a option

  (** update or add the session data associated to the givent key *)
  val set_session_data : t -> 'a key -> 'a -> unit

  (** remove the session data associated to the givent key *)
  val remove_session_data : t -> 'a key -> unit

  (** remove all server side and client side session data by expiring the
      session cookies. *)
  val delete_session : ?cookie_policy:cookie_policy -> 'a Request.t -> Cookies.t

  (** For debugging *)
  val get_session_key : t -> string
  val get_session_addr : t -> string
end

(** HTML: generated by chaml and vfs_pack *)
module Html : sig
  (** Module type for filling an input buffer by printing. A module of
    this type is openned in [<ML>] section of [.chaml] file when
    using [vfs_pack]. *)
  module type Output = sig
    val echo : string -> unit
    val printf : ('a, Format.formatter, unit, unit) format4 -> 'a
  end

  (** type of html elements as string *)
  type elt = (module Output) -> unit

  (** type of a chaml page, as generated by chaml preprocessor and to be
      given as argument to {!Simple_httpd.Server.add_route_handler_chaml} *)
  type chaml = string Request.t -> Headers.t -> Headers.t * Cookies.t * Input.t
end

(** Module to create a server and run it *)
module Server : sig
  (** {1 Main Server type} *)

  type t
  (** A HTTP server. See {!create} for more details. *)

  module type Parameters = sig
    val max_connections : int ref
    (** Maximum number of simultaneous connections (default: 32). Remember
        that because of timeout, a lot of connections car remain opened. *)

    val num_threads : int ref
    (** Number of system thread (ocaml domains) treating request. One more
        thread is used to accept connection. The default is
        [Domain.recommended_domain_count () - 1)]*)

    val timeout : float ref
    (** Timeout before closing a connection in case of inactivity (default
        300s = 5m). Currently timeout is only in to set socket option
        SO_RCVTIMEO and SO_SNDTIMEO. Small timeout and the capacity of
        Simple_httpd to handle many request should mitigate "slow lorris"
        attacks. A timeout for the total request, or something else time
        could be added in the future for better defense. *)

    val buf_size : int ref
    (** Initial size of the buffer allocated by each client to parse request *)

    val ssl_reload_period : int -> unit
    (** Set ssl period in seconds at which all ssl certificates are checked for
        renewal (default 1 day)*)

    val restart_file : string ref
    (** Name of a file where to save information when restarting.

        Note: the file is not encrypted and contains sensitive data. It is
        normally deleted after a server restart, but you shoud be careful not
        to let such a file escape. *)

    val log_requests : int ref
    (** log level for requests information *)

    val log_exceptions : int ref
    (** log level for exceptions *)

    val log_scheduler : int ref
    (** log level to debug the scheduler *)

    val log_authentications : int ref
    (** log level for authentications *)

    val log_processes : int ref
    (** log level for processes *)

    val log_user : int ref
    (** log level for user specific logs *)

    val log_folder : string ref
    (** if non empty, one log per domain will be written in the given
        folder. The filename of the log is [log_basename-domainid.log]. Log
        are recreated if deleted while the server is running to be compatible
        with logrotate. *)

    val log_basename : string ref
    (** basename for the log file (see above. default: [Filename.basename
        Sys.argv[0]]. *)

    val log_perm : int ref
    (** permission when creating log files and folder (default 0o700). *)
  end

  val args : unit -> (Arg.key * Arg.spec * Arg.doc) list * (module Parameters)
  (** The defaut command line options. The idea is that all server using
      Simple_httpd would share a common options set, making it easier to
      use.

      [args ()] returns a spec list suitable for OCaml's Arg module with a
      module of type {!Parameters} whose references will be modified when parsing
      the options.

      The value of args corresponds to the following options:
      {[
  --buf-size         set the size of the buffer used for input and output (one per client) (default 32ko)
  --ktls             use ktls over ssl (default false)
  --log-requests     log level for requests (default 1)
  --log-exceptions   log level for exceptions (default 1)
  --log-scheduler    log level for scheduler debug (default 0)
  --log-folder       log folder (default none)
  --log-basename     log basename (default basename of argv[0])
  --log-perm         log permission (default 0o700)
  --max-connections  maximum number of simultaneous connections (default 32)
  -c                 maximum number of simultaneous connections (default 32)
  --nb-threads       maximum number of threads (default
  -j                 maximum number of threads
  --timeout          timeout in seconds, connection is closed after timeout second of inactivity (default: 300)
  -help              Display this list of options
  --help             Display this list of options
      ]}
   *)

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

  val started_time : t -> float
  (** Returns the time the server started *)

  val num_threads : t -> int
  (** Number of threads used by the server *)

  val max_connections : t -> int
  (** Maximum number of connections allowed by the server *)

  (** {1 Route handlers}

      Here are the main function to explain what you server should to depending
      on the url send by the client.
   *)

  val add_route_handler :
    ?addresses:Address.t list ->
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
      @param meth if provided, only accept requests with the given method.
        Typically one could react to [`GET] or [`PUT].
      @param filter can be used to modify the request and response and also
        to reject some request using {!Response.fail_raise}. The default filter
        accept all requests and does not do any transformation.
   *)

  val add_route_handler_stream :
    ?addresses:Address.t list ->
    ?meth:Method.t ->
    ?filter:Input.t Filter.t ->
    t ->
    ('a, Input.t Request.t -> Response.t) Route.t -> 'a ->
    unit
  (** Similar to {!add_route_handler}, but where the body of the request
      is a stream of bytes that has not been read yet.
      This is useful when one wants to stream the body directly into a parser,
      json decoder (such as [Jsonm]) or into a file. *)

  val add_route_handler_chaml :
    ?addresses:Address.t list ->
    ?meth:Method.t ->
    ?filter:Input.t Filter.t ->
    t -> ('a, Html.chaml) Route.t -> 'a -> unit

  val redirect_https :
    ?addresses:Address.t list ->
    ?filter:Input.t Filter.t ->
    t -> unit
  (** Emmit a permanent redirect response for the same request, but in https.
      The Host header must be present in the request, otherwise it gives a
      not_found response code *)

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
    ?filter:Input.t Filter.t ->
    ?prefix:string ->
    ?config:config ->
    dir:string ->
    Server.t -> unit
  (** [add_dirpath ~config ~dir ~prefix server] adds route handle to the
      [server] to serve static files in [dir] when url starts with [prefix],
      using the given configuration [config]. Method is always [GET].
    @param addresses like for {!Server.add_route_handler}
    @param filter like for {!Server.add_route_handler}
   *)

  (** {2 Virtual file system.}

      This is used to emulate a file system from pure OCaml functions and
      data, e.g. for resources bundled (totally of partially) inside the web
      server memory. *)

  (** {!VFS} allows to serve dynamic content produced using such a record *)
  type dynamic = Html.chaml
     (** dynamic content can set then headers, cookies and input of the
         response.  If initial headers contain [Headers.Cache_Control] or
         [Headers.Etag]. *)

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
    type path
    (** type of path for the VFS *)

    val path : string list -> path
    (** convert url components to path *)

    val concat : path -> string -> path
    (** extend a path *)

    val to_string : ?prefix:string -> path -> string
    (** convert a path back to a url, or string for log *)

    val descr : string
    (** Description of the VFS *)

    val is_directory : path -> bool

    val contains : path -> bool
    (** [file_exists vfs path] returns [true] if [path] points to a file
        or directory inside [vfs]. *)

    val list_dir : path -> string array
    (** List directory. This only returns basenames, the files need
        to be put in the directory path using [Filename.concat]. *)

    val delete : path -> unit
    (** Delete path *)

    val create : path -> (bytes -> int -> int -> unit) * (unit -> unit)
    (** Create a file and obtain a pair [write, close] *)

    val read_file : path -> file_info
    (** Read content of a file. *)

    val keep : path -> unit
    (** Prefent VFS.free to close the file descriptor *)

    val free : path -> unit
    (** some path may contain fs ressources that must br freed *)
  end

  val vfs_of_dir : string -> (module VFS)
  (** [vfs_of_dir dir] makes a virtual file system that reads from the
      disk.
   *)

  val add_vfs :
    ?addresses: Address.t list ->
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

(** A module for authenticating user on the site *)
module Auth : sig
  module type Login = sig
    (** type of the information about user *)
    type t

    (** function validating the password. Returns [None] for bad
        login/password. Returns the information about user otherwise. *)
    val check : login:string -> password:string -> t option

    (** The url of the login page. It should return a POST request with two
        values: ["login"] of type string and ["password"] of type string
        too. Password should be encrypted by the [check] function, for
        instance using [Digest.string]. The login_url can be the url of the
        [login_page] provided by the functor below. *)
    val login_url : string

    (** Cookie policy, check {!Session.cookie_policy}*)
    val cookie_policy : Session.cookie_policy

  end

  module Make(Login:Login) : sig
    (** a basic default login page that you may associate to the given
        login_url.  If will both validate login and display the given page if
        the user is not logged.  The page can be adapted using the optional
        argument, either giving your own page or just the link of a css.  In
        case of succesful login, the user is redirected to the destination
        (empty url = same page, by default) *)
    val login_page : ?destination:string -> ?css:string -> ?page:Html.chaml
                     -> Html.chaml

    (** logout by destroying the login session data and cookies and send back
        to the destination [login_url] by default. If a page is provided,
        there is no redirection, the given page is directly displayed. *)
    val logout_page : ?destination:string -> ?page:Html.chaml -> Html.chaml

    (** checking session, with proper type for defaut parameter of {!Server} functions *)
    val check : ?nologin:(Response_code.t * Headers.t * string) ->
                ?check_data:(Login.t -> bool)
                -> 'a Request.t -> (Cookies.t * Session.t)

    (** A way to check the session and get the data associated to the session
        in the handler. One may also give a different response when not logged *)
    val check_handler : ?not_logged:('a Request.t -> Response.t)
      -> ('a Request.t -> Cookies.t -> Login.t -> Response.t)
      -> ('a Request.t -> Response.t)

    (** check session as a {!Filter} *)
    val check_filter :
      ?nologin:(Response_code.t * Headers.t * string) ->
      ?check_data:(Login.t -> bool)
      -> 'a Filter.t

    val get_session_data : 'a Request.t -> Login.t option

    val auth_key : Login.t Session.key
  end
end

(** A module to get detail status about the server *)
module Status : sig
  val html : ?log_size:int ->
             ?in_head:Html.elt -> ?css:string ->
             ?start_header: Html.elt -> ?end_header: Html.elt ->
             ?start_contents: Html.elt -> ?end_contents: Html.elt ->
             Server.t -> Html.chaml
(** Returns a detailed server status as html, including

    {ul {- number of actives connections (total and per threads)}
        {- memory and cpu usage}
        {- hd usage}
        {- number of used file descriptor}}

    We are planning to make this page extensible with your own date...

    If the server uses a [log_folder], the given number of lines of the log is
    given for each thread/domain. If [log_size] is not provider, the value of
    the parameter ["nb"] of the query will be used, and if it not provided or
    is not an integer, 100 is used.

    You may add content in the page, in the same way as for the {!Stats}
    module below. *)
end

(** provide a filter giving very simple statistics. We can do much better
    but be carefull on how to do it *)
module Stats : sig

  (** This a filter to acquire statistics.
    [let (filter, get) = Stats.filter ()]
    will give you a [Filter.t] and a function [get] returning the statistics
    as a html page
    ["N requests (average response time:
       Tms = T1ms (read) + T2ms (build) + T3ms (send))"]

       The structure of the page uses the optionnal parameters as:
       <head>
          ...
          link css if given
          in_head
       </head>
       <body>
         <header>
           start_header if given
           ...
           end_header if given
         </header>
         <div class="content">
           start_contents if given
           ...
           end_contents if given
         </div>
       </body>
   *)
  val filter : unit -> 'a Filter.t *
                         (?in_head: Html.elt -> ?css:string ->
                          ?start_header:Html.elt -> ?end_header:Html.elt ->
                          ?start_contents:Html.elt -> ?end_contents:Html.elt ->
                          Html.chaml)
end

(** Hight level module to write server handling multiple hosts/addresses *)
module Host : sig
  open Server
  open Dir

  (** A module of this type is provided to Init functor to allow you to
      initialize the route of your host *)
  module type Init = sig
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

    val add_route_handler_chaml :
      ?meth:Method.t ->
      ?filter:Input.t Filter.t ->
      ('a, Html.chaml) Route.t -> 'a -> unit

    val redirect_https : ?filter:Input.t Filter.t -> unit -> unit
    (** Same as Server.redirect_https but the server is provided when
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

    module Init(_:Init) : sig end
    (** This function must initialize all the route of your server.*)
  end

  val start_server : (module Server.Parameters) -> (module Host) list -> unit
  (** start a server with the given list of Host *)
end
