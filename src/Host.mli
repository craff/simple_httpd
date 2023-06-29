open Server
open Dir

module type HostInit = sig
  val server : t

  val add_route_handler :
    ?meth:Method.t ->
    ?filter:Input.t Route.filter ->
    ('a, string Request.t -> Response.t) Route.t -> 'a -> unit

  val add_route_handler_stream :
    ?meth:Method.t ->
    ?filter:Input.t Route.filter ->
    ('a, Input.t Request.t -> Response.t) Route.t -> 'a -> unit

  val add_dir_path :
    ?filter:Input.t Route.filter ->
    ?prefix:string ->
    ?config:config ->
    string -> unit

  val add_vfs :
    ?filter:Input.t Route.filter ->
    ?prefix:string ->
    ?config:config ->
    (module VFS) -> unit
end

module type Host = sig
  val addresses : Address.t list
  val hostnames : string list

  module Init(_:HostInit) : sig end
end

val start_server : (module Server.Parameters) -> (module Host) list -> unit
