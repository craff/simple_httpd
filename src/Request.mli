(** {1 Requests}

    Requests are sent by a client, e.g. a web browser, curl or wget. *)

type 'body t = { meth: Method.t (** The method of this request *)
               ; host: string (** The host header *)
               ; client: Async.client
               (** information about the client used by the scheduler *)
               ; headers: Headers.t (** Request headers *)
               ; cookies: Cookies.t (** Request cookies *)
               ; http_version: int*int (** protocol set in the request *)
               ; path: string (** full path of the request *)
               ; path_components: string list  (** the part of the path that precedes [query] and is split on ["/"]. *)
               ; query: (string*string) list (** the query parameters in ["?foo=bar,x=y"] *)
               ; body: 'body (** The body (its current state, depending on the stage of treatment *)
               ; start_time: float (** unix time when the request was started *)
               ; trailer: (Headers.t * Cookies.t) option ref (** trailer after a chunked body.
                                                                 this is only updated after the body was fully parsed. *)
               }
(** A request with method, path, host, headers, and a body, sent by a client.

    The body is polymorphic because the request goes through several
    transformations. First it a body with a unread {!Simple_httpd.Input.t}
    stream, as only the request and headers are read; while the body might
    be entirely read as a string via {!read_body_full}.  *)

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
(** Modify headers *)

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

val host : _ t -> string
(** Host field of the request. It also appears in the headers. *)

val meth : _ t -> Method.t
(** Method for the request. *)

val path : _ t -> string
(** Request path (including query). *)

val path_components : _ t -> string list
(** Request path components without query *)

val client : _ t -> Async.client
(** Request client *)

val query : _ t -> (string*string) list
(** Decode the query part of the {!field-path} field *)

val body : 'b t -> 'b
(** Request body, possibly empty. *)

val start_time : _ t -> float
(** time stamp (from [Unix.gettimeofday]) after parsing the first line of
    the request *)

val reset_timeout : _ t -> unit
(** reset the timeout for a request if it needs time *)

val trailer : _ t -> (Headers.t * Cookies.t) option
(** trailer, read after a chunked body. Only maeningfull after the body stream
    we fully read and closed *)

val limit_body_size : max_size:int -> Input.t t -> Input.t t
(** Limit the body size to [max_size] bytes, or return a [413] error. *)

val read_body_full : buf:Buffer.t -> Input.t t -> string t
(** Read the whole body into a string. Potentially blocking.

    @param buf_size initial size of underlying buffer *)

val close_after_req : _ t -> bool
(** Tells if we are supposed to close the connection after answering the request *)

val parse_req_start :  client:Async.client -> buf:Buffer.t ->
                       Input.t -> Input.t t option
val parse_body_ : tr_stream:(Input.t -> Input.t) ->
                  buf:Buffer.t -> Input.t t -> Input.t t
val parse_body : buf:Buffer.t -> Input.t t -> Input.t t
