
type buf = Buffer.t

let log      = Log.f

module type SERVER_SENT_GENERATOR = sig
  val set_headers : Headers.t -> unit
  val send_event :
    ?event:string ->
    ?id:string ->
    ?retry:string ->
    data:string ->
    unit -> unit
  val close : unit -> unit
end
type server_sent_generator = (module SERVER_SENT_GENERATOR)

type t = {
  listens : Address.t array;

  timeout: float;

  num_thread: int;

  max_connections: int;

  masksigpipe: bool;

  buf_size: int;

  status : Async.status;

  handlers : Route.handlers
}

let listens self = self.listens

let status self = self.status

let active_connections _self = failwith "unimplemented"

let add_route_handler ?addresses ?hostnames ?meth ?filter
    self route f : unit =
  let tr_req _oc req ~resp f =
    resp (f (Request.read_body_full ~buf:(Request.client req).buf req))
  in
  Route.add_route_handler ?filter ?addresses ?hostnames ?meth
    self.handlers route ~tr_req f

let add_route_handler_stream ?addresses ?hostnames ?meth ?filter self route f =
  let tr_req _oc req ~resp f = resp (f req) in
  Route.add_route_handler ?filter ?addresses ?hostnames ?meth
    self.handlers route ~tr_req f

let[@inline] _opt_iter ~f o = match o with
  | None -> ()
  | Some x -> f x

let add_route_server_sent_handler ?filter self route f =
  let tr_req oc req ~resp f =
    let buf = (Request.client req).buf in
    let req = Request.read_body_full ~buf req in
    let headers = ref Headers.(empty |> set Content_Type "text/event-stream") in

    (* send response once *)
    let resp_sent = ref false in
    let send_response_idempotent_ () =
      if not !resp_sent then (
        resp_sent := true;
        (* send 200 response now *)
        let initial_resp = Response.make_void ~headers:!headers ~code:200 () in
        resp initial_resp;
      )
    in

    let send_event ?event ?id ?retry ~data () : unit =
      send_response_idempotent_();
      _opt_iter event ~f:(fun e -> Output.printf oc "event: %s\n" e);
      _opt_iter id ~f:(fun e -> Output.printf oc "id: %s\n" e);
      _opt_iter retry ~f:(fun e -> Output.printf oc "retry: %s\n" e);
      let l = String.split_on_char '\n' data in
      List.iter (fun s -> Output.printf oc "data: %s\n" s) l;
      Output.add_char oc '\n'; (* finish group *)
      Output.flush oc;
    in
    let module SSG = struct
      let set_headers h =
        if not !resp_sent then (
          headers := List.rev_append h !headers;
          send_response_idempotent_()
        )
      let send_event = send_event
      let close () = raise Exit
    end in
    try f req (module SSG : SERVER_SENT_GENERATOR);
    with Exit -> Output.close oc
  in
  Route.add_route_handler self.handlers ?filter ~meth:GET route ~tr_req f

let create
    ?(masksigpipe=true)
    ?(max_connections=32)
    ?(num_thread=Domain.recommended_domain_count () - 1)
    ?(timeout=300.0)
    ?(buf_size=16 * 2048)
    ?(listens = [Address.make ()])
    () : t =
  let max_connections = max 4 max_connections in
  if num_thread <= 0 || max_connections < num_thread then
    invalid_arg "bad number of threads or max connections";
  let status = Async.{
      nb_connections = Array.init num_thread (fun _ -> Atomic.make 0)
    }
  in
  let (listens, handlers) =
    Address.register Route.empty_handler listens
  in
  let self = {
    listens; masksigpipe; buf_size;
    max_connections;
    handlers; timeout; num_thread;
    status
    }
  in
  self

let handle_client_ (self:t) (client:Async.client) : unit =
  let buf = client.buf in
  let oc  = Output.create ~buf_size:self.buf_size client in
  let is = Input.of_client ~buf_size:self.buf_size client in
  let continue = ref true in
  while !continue do
    match Request.parse_req_start ~client ~buf is with
    | None ->
      continue := false (* client is done *)

    | Some req ->
      log ~lvl:2 (fun k->k "req: %s" (Format.asprintf "@[%a@]" Request.pp_ req));

      if Request.close_after_req req then continue := false;

      try
        (* is there a handler for this path? *)
        let (req, filter, handler) = Route.find self.handlers req in
        (* handle expect/continue *)
        begin match Request.get_header ~f:String.trim req Headers.Expect with
          | Some "100-continue" ->
            log ~lvl:2 (fun k->k "send back: 100 CONTINUE");
            Response.output_ oc (Response.make_raw ~code:100 "");
          | Some s -> Response.fail_raise ~code:417 "unknown expectation %s" s
          | None -> ()
        end;

        (* now actually read request's body into a stream *)
        let req =
          Request.parse_body_ ~tr_stream:(fun s->s) ~buf req
        in

        (* how to reply *)
        let resp r =
          let r = filter r in
          try
            if Headers.get Headers.Connection r.Response.headers = Some"close" then
              continue := false;
            Response.output_ oc r;
          with Sys_error _
             | Unix.Unix_error _ as e ->
                continue := false;
                log ~lvl:1 (fun k -> k "fail to output response (%s)"
                                           (Async.printexn e))
        in
        (* call handler *)
        handler oc req ~resp;
        log ~lvl:1 (fun k -> k "response sent after %fms" (1e3 *. (Unix.gettimeofday () -. req.start_time)));
        if !continue then Async.yield ()
      with
      | Sys_error _ | Unix.Unix_error _ | Async.ClosedByHandler | Async.TimeOut as e ->
         log ~lvl:1 (fun k -> k "broken connection (%s)"
                                    (Async.printexn e));
         continue := false; (* connection broken somehow *)

      | Headers.Bad_req (c,s,headers,cookies) ->
         log ~lvl:1 (fun k -> k "redirect request (%s)" s);
         let res = Response.make_raw ~headers ~cookies ~code:c s in
         begin
           try Response.output_ oc res
           with Sys_error _ | Unix.Unix_error _ -> ()
         end;
         if not (c < 500) then continue := false else Async.yield ()

      | e ->
         log ~lvl:1 (fun k -> k "server error (%s)"
                                    (Async.printexn e));
         continue := false;
         Response.output_ oc @@
           Response.fail ~code:500 "server error: %s" (Async.printexn e)
  done;
  log ~lvl:2 (fun k->k "done with client, exiting");
  ()

let run (self:t) =
  let handler client_sock = handle_client_ self client_sock in
  let maxc = self.max_connections in
  let a = Async.run ~nb_threads:self.num_thread ~listens:self.listens
            ~maxc ~timeout:self.timeout ~status:self.status
            handler
  in
  Array.iter (fun d -> Domain.join d) a
