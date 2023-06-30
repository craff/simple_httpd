(** {1 Routing}

    Basic type-safe routing. *)

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
    to function like {!add_route_handler}.

    The transformation of the response may depend on the request, Hence the
    type. For instance the filter provided by the optional module
    {{:../../simple_httpd_camlzip/Simple_httpd_camlzip/index.html}Simple_httpd_camlzip} uses this to compress the
    response only if [deflate] is allowed using the header named
    {!Headers.Accept_Encoding}. *)

module Filter : sig
  type 'a t = 'a Request.t -> 'a Request.t * (Response.t -> Response.t)

  val decode_request : ('a -> 'a) -> (Headers.t -> Headers.t)
                       -> 'a t
  (** helper to create a t transforming only the request. *)

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

type handler
type handlers = handler array

type path = string list (* split on '/' *)

type 'a treatment = Output.t ->
                    Input.t Request.t ->
                    resp:(Response.t -> unit) -> 'a

val add_route_handler :
  ?addresses:Address.t list ->
  ?hostnames:string list ->
  ?meth:Method.t ->
  ?filter:Input.t Filter.t ->
  tr_req:('a -> unit) treatment ->
  handlers -> ('b, 'a) t -> 'b -> unit

val empty_handler : 'a -> handler

val find : handlers -> Input.t Request.t ->
           (Input.t Request.t * (Response.t -> Response.t) * unit treatment)
