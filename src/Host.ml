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

let collect_addresses (hosts : (module Host) list) : Address.t list =
  let open Address in
  let res = ref [] in
  let add addr =
    try
      let addr' =
        List.find (fun a -> a.addr = addr.addr && a.port = addr.port) !res
      in
      if addr.ssl <> addr'.ssl then
        failwith "addresses with incompatible certificate";
      if addr.reuse <> addr'.reuse then
        failwith "addresses with incompatible reuse option";
    with Not_found -> res := addr :: !res
  in
  List.iter (fun (module Host:Host) -> List.iter add Host.addresses) hosts;
  !res

let start_server
    ?masksigpipe ?max_connections ?num_thread ?timeout ?buf_size
    (hosts : (module Host) list) =
  let addresses = collect_addresses hosts in
  let server =
    create ?masksigpipe ?max_connections ?num_thread ?timeout ?buf_size
      ~listens:addresses ()
  in
  List.iter (fun (module Host:Host) ->
      let module I = struct
          open Host
          let server = server
          let add_route_handler ?meth ?filter route fn =
            add_route_handler ~addresses ~hostnames ?meth ?filter server route fn

          let add_route_handler_stream  ?meth ?filter route =
            add_route_handler_stream ~addresses ~hostnames ?meth ?filter server route

          let add_dir_path ?filter ?prefix ?config dir =
            add_dir_path ~addresses ~hostnames ?filter ?prefix ?config ~dir server

          let add_vfs ?filter ?prefix ?config vfs =
            add_vfs ~addresses ~hostnames ?filter ?prefix ?config ~vfs server
      end in
      let module _ = Host.Init(I) in
      ()) hosts;

  Array.iter (fun l ->
      let open Address in
      Log.f ~lvl:0 (fun k -> k "listening on http://%s:%d" l.addr l.port))
    (listens server);

  run server