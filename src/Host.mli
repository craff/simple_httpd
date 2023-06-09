open Simple_httpd_server
open Simple_httpd_dir

module type HostInit = sig
  val server : t

  val add_route_handler :
    ?meth:Meth.t ->
    ?filter:filter ->
    ('a, string Request.t -> Response.t) Route.t -> 'a -> unit

  val add_route_handler_stream :
    ?meth:Meth.t ->
    ?filter:filter ->
    ('a, byte_stream Request.t -> Response.t) Route.t -> 'a -> unit

  val add_dir_path :
    ?filter:Simple_httpd_server.filter ->
    ?prefix:string ->
    ?config:config ->
    string -> unit

  val add_vfs :
    ?filter:Simple_httpd_server.filter ->
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
