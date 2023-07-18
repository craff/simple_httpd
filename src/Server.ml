open Response_code

type buf = Buffer.t
let log = Log.f

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

  num_threads: int;

  max_connections: int;

  buf_size: int;

  status : Async.status;

  handlers : Route.handlers
}

let listens self = self.listens

let status self = self.status

let num_threads self = self.num_threads

let max_connections self = self.max_connections

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

let add_route_server_sent_handler ?addresses ?hostnames ?filter self route f =
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
        let initial_resp = Response.make_void ~headers:!headers ~code:ok () in
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
  Route.add_route_handler ?filter ?addresses ?hostnames
    ~meth:GET self.handlers route ~tr_req f

module type Parameters = sig
  val max_connections : int ref
  val num_threads : int ref
  val timeout : float ref
  val buf_size : int ref
  val ssl_reload_period : int -> unit

  val log_requests : int ref
  val log_exceptions : int ref
  val log_scheduler : int ref
  val log_folder : string ref
  val log_basename : string ref
  val log_perm : int ref
end

let args () =
  let module Params = struct
      let max_connections = ref 32
      let num_threads = ref (Domain.recommended_domain_count () - 1)
      let timeout = ref 300.0
      let buf_size = ref (8 * 4_096)
      let ssl_reload_period = Address.set_ssl_reload_period

      let log_requests   = ref 1
      let log_scheduler  = ref 0
      let log_exceptions = ref 1
      let log_folder = ref ""
      let log_basename = ref (Filename.remove_extension
                                (Filename.basename Sys.argv.(0)))
      let log_perm = ref 0o700
    end in
  let open Params in
  let open Arg in
  ([
      ( "--buf-size", Set_int buf_size,
        " set the size of the buffer used for input and output (one per client)");
      ( "--log-requests", Set_int log_requests,
        " log level for requests (default 1)");
      ( "--log-exceptions", Set_int log_exceptions,
        " log level for exceptions (default 1)");
      ( "--log-scheduler", Set_int log_scheduler,
        " log level for scheduler debug (default 0)");
      ( "--log-folder", Set_string log_folder, " log folder (default none)");
      ( "--log-basename", Set_string log_basename,
        " log basename (default basename of argv[0])");
      ( "--log-perm", Set_int log_perm, " log permission (default 0o700)");
      ( "--max-connections", Set_int max_connections,
        " maximum number of simultaneous connections (default 32)");
      ( "--ssl-reload-period", Int ssl_reload_period,
        " period, in seconds, at which all ssl certificates are checked for renewal (default 1 day)");
      ( "-c", Set_int max_connections, " same as --max-connections");
      ( "--nb-threads", Set_int num_threads,
        " maximum number of threads (default nbcore - 1)");
      ( "-j", Set_int num_threads, " same as --nb-threads");
      ( "--timeout", Set_float timeout,
        " timeout in seconds, connection is closed after timeout second of inactivity (default: 32)");
    ], (module Params : Parameters))

let create ?(listens = [Address.make ()]) (module Params : Parameters) =
  let open Params in
  let num_threads = !num_threads in
  let max_connections = !max_connections in
  let buf_size = !buf_size in
  let timeout = !timeout in

  Log.set_log_requests !log_requests;
  Log.set_log_scheduler !log_scheduler;
  Log.set_log_exceptions !log_exceptions;
  if !log_folder <> "" then
    Log.set_log_folder ~basename:!log_basename ~perm:!log_perm
      !log_folder (num_threads + 1);
  let max_connections = max 4 max_connections in
  if num_threads <= 0 || max_connections < num_threads then
    invalid_arg "bad number of threads or max connections";
  let status = Async.{
        nb_connections = Array.init num_threads (fun _ -> Atomic.make 0);
        domain_ids = Array.init num_threads (fun _ -> Domain.self ());
          (* will be changed when the threads are spawn *)
    }
  in
  let (listens, handlers) =
    Address.register Route.empty_handler listens
  in
  let self = { listens; buf_size; max_connections
             ; handlers; timeout; num_threads; status }
  in
  self

let handle_client_ (self:t) (client:Async.client) : unit =
  let buf = client.buf in
  let oc  = Output.create ~buf_size:self.buf_size client in
  let is = Input.of_client ~buf_size:self.buf_size client in
  let cont = ref true in
  while !cont do
    match Request.parse_req_start ~client ~buf is with
    | None ->
      cont := false (* client is done *)

    | Some req ->
      log (Req 1) (fun k->k "req: %s" (Format.asprintf "@[%a@]" Request.pp_ req));

      if Request.close_after_req req then cont := false;

      try
        (* is there a handler for this path? *)
        let (req, filter, handler) = Route.find self.handlers req in
        (* handle expect/continue *)
        begin match Request.get_header ~f:String.trim req Headers.Expect with
          | Some "100-continue" ->
            log (Req 1) (fun k->k "send back: 100 CONTINUE");
            Response.output_ oc (Response.make_raw ~code:continue "");
            (* CHECK !!! *)
          | Some s -> Response.fail_raise ~code:expectation_failed
                        "unknown expectation %s" s
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
              cont := false;
            Response.output_ oc r;
          with Sys_error _
             | Unix.Unix_error _ as e ->
                cont := false;
                log (Exc 1) (fun k -> k "fail to output response (%s)"
                                           (Async.printexn e))
        in
        (* call handler *)
        handler oc req ~resp;
        log (Req 0) (fun k -> k "response sent after %fms" (1e3 *. (Unix.gettimeofday () -. req.start_time)));
        if !cont then Async.yield ()
      with
      | Headers.Bad_req (c,s,headers,cookies) ->
         log (Req 0) (fun k -> k "not 200 status: %d (%s)" (c :> int) s);
         let res = Response.make_raw ~headers ~cookies ~code:c s in
         begin
           try Response.output_ oc res
           with Sys_error _ | Unix.Unix_error _ -> ()
         end;
         if not ((c :> int) < 500) then cont := false else Async.yield ()

      | Sys_error _ | Unix.Unix_error _
        | Ssl.Write_error _ | Ssl.Read_error _
        | Async.ClosedByHandler | Async.TimeOut as e ->
         log (Exc 1) (fun k -> k "probably broken connection (%s)"
                                    (Async.printexn e));
         cont := false; (* connection broken somehow *)

      | e ->
         log (Exc 0) (fun k -> k "internal server error (%s)"
                                    (Async.printexn e));
         cont := false;
         Response.output_ oc @@
           Response.fail ~code:internal_server_error
             "server error: %s" (Async.printexn e)
  done;
  log (Sch 0) (fun k->k "done with client, exiting");
  ()

let run (self:t) =
  let handler client_sock = handle_client_ self client_sock in
  let maxc = self.max_connections in
  let a = Async.run ~nb_threads:self.num_threads ~listens:self.listens
            ~maxc ~timeout:self.timeout ~status:self.status
            handler
  in
  Array.iter (fun d -> Domain.join d) a
