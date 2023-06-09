open Server
open Dir

module type HostInit = sig
  val server : t

  val add_route_handler :
    ?meth:Method.t ->
    ?filter:Route.filter ->
    ('a, string Request.t -> Response.t) Route.t -> 'a -> unit

  val add_route_handler_stream :
    ?meth:Method.t ->
    ?filter:Route.filter ->
    ('a, Input.t Request.t -> Response.t) Route.t -> 'a -> unit

  val add_dir_path :
    ?filter:Route.filter ->
    ?prefix:string ->
    ?config:config ->
    string -> unit

  val add_vfs :
    ?filter:Route.filter ->
    ?prefix:string ->
    ?config:config ->
    (module VFS) -> unit
end

module type Host = sig
  val addresses : Address.t list
  val hostnames : string list

  module Init(_:HostInit) : sig end
end

val start_server :
  ?masksigpipe:bool ->
  ?max_connections:int ->
  ?num_thread:int ->
  ?timeout:float ->
  ?buf_size:int -> (module Host) list -> unit
