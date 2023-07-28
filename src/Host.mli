open Server
open Dir

module type Init = sig
  val server : t

  val add_route_handler :
    ?meth:Method.t ->
    ?filter:Input.t Route.Filter.t ->
    ('a, string Request.t -> Response.t) Route.t -> 'a -> unit

  val add_route_handler_stream :
    ?meth:Method.t ->
    ?filter:Input.t Route.Filter.t ->
    ('a, Input.t Request.t -> Response.t) Route.t -> 'a -> unit

  val add_route_handler_chaml :
    ?meth:Method.t ->
    ?filter:Input.t Route.Filter.t ->
    ('a, Html.chaml) Route.t -> 'a -> unit

  val add_dir_path :
    ?filter:Input.t Route.Filter.t ->
    ?prefix:string ->
    ?config:config ->
    string -> unit

  val add_vfs :
    ?filter:Input.t Route.Filter.t ->
    ?prefix:string ->
    ?config:config ->
    (module VFS) -> unit
end

module type Host = sig
  val addresses : Address.t list

  module Init(_:Init) : sig end
end

val start_server : (module Server.Parameters) -> (module Host) list -> unit
