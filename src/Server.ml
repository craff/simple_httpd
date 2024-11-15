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

  handlers : Route.handlers;

  started_time : float;

  restart_file : string;

  mutable domains : Domain.id Array.t;
}

let listens self = self.listens

let num_threads self = self.num_threads

let max_connections self = self.max_connections

let started_time self = self.started_time

let domains self = self.domains

let add_route_handler ?addresses ?meth ?filter
    self route f : unit =
  let tr_req _oc req ~resp f =
    resp (f (Request.read_body_full ~buf:(Request.client req).buf req))
  in
  Route.add_route_handler ?filter ?addresses ?meth
    self.handlers route ~tr_req f

let add_route_handler_stream ?addresses ?meth ?filter self route f =
  let tr_req _oc req ~resp f = resp (f req) in
  Route.add_route_handler ?filter ?addresses ?meth
    self.handlers route ~tr_req f

let add_route_handler_chaml ?addresses ?meth ?filter self route f =
  let headers = [(Headers.Cache_Control, "no-store");
                 (Headers.Content_Type, "text/html")] in
  let tr_req _oc req ~resp f  =
    let req = Request.read_body_full ~buf:(Request.client req).buf req in
    let (headers, cookies, stream) = f req headers in
    let r = Response.make_stream ~headers ~cookies stream in
    resp r
  in
  Route.add_route_handler ?filter ?addresses ?meth
    self.handlers route ~tr_req f

let redirect_https ?addresses ?filter self =
  add_route_handler ?addresses ?filter self Route.rest
    (fun _ req ->
      try
        let host = match Request.get_header req Headers.Host with
          | Some h -> h
          | None -> raise Not_found
        in
        let path = Request.path req in
        let url = Printf.sprintf "https://%s%s" host path in
        let headers = [ (Headers.Location, url) ] in
        Response.fail ~headers ~code:Response_code.permanent_redirect
          "Redirection to https"
      with
        _ -> Response.fail_raise ~code:Response_code.not_found "Not_found")

let[@inline] _opt_iter ~f o = match o with
  | None -> ()
  | Some x -> f x

let add_route_server_sent_handler ?addresses ?filter self route f =
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
  Route.add_route_handler ?filter ?addresses
    ~meth:GET self.handlers route ~tr_req f

module type Parameters = sig
  val max_connections : int ref
  val num_threads : int ref
  val timeout : float ref
  val buf_size : int ref
  val ssl_reload_period : int -> unit

  val restart_file : string ref

  val log_requests : int ref
  val log_exceptions : int ref
  val log_scheduler : int ref
  val log_authentications : int ref
  val log_processes : int ref
  val log_user : int ref

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

      let restart_file = ref ""

      let log_requests   = ref 1
      let log_scheduler  = ref 0
      let log_exceptions = ref 1
      let log_authentications = ref 1
      let log_processes = ref 1
      let log_user = ref 1
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
      ( "--log-authentications", Set_int log_authentications,
        " log level for authentications (default 1)");
      ( "--log-scheduler", Set_int log_scheduler,
        " log level for scheduler debug (default 0)");
      ( "--log-processes", Set_int log_processes,
        " log level for external processes logs (default 1)");
      ( "--log-user", Set_int log_user,
        " log level for user specific logs (default 1)");
      ( "--log-folder", Set_string log_folder, " log folder (default none)");
      ( "--log-basename", Set_string log_basename,
        " log basename (default basename of argv[0])");
      ( "--log-perm", Set_int log_perm, " log permission (default 0o700)");
      ( "--restart-file", Set_string restart_file,
        " name of file to save sessions and other information when restarting");
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
  Log.set_log_authentications !log_authentications;
  Log.set_log_processes !log_processes;
  Log.set_log_user !log_user;

  if !log_folder <> "" then
    Log.set_log_folder ~basename:!log_basename ~perm:!log_perm
      !log_folder (num_threads + 1)
  else
    Log.init_log_folder (num_threads + 1);

  let max_connections = max 4 max_connections in
  if num_threads <= 0 || max_connections < num_threads then
    invalid_arg "bad number of threads or max connections";
  let (listens, handlers) =
    Address.register Route.empty_handler listens
  in
  let started_time = Unix.gettimeofday () in
  let restart_file = !restart_file in
  let domains = [||] in (* set when running the server *)
  let self = { listens; buf_size; max_connections; started_time
             ; handlers; timeout; num_threads; restart_file; domains }
  in
  self


let handle_client_ (self:t) (client:Async.client) : unit =
  let buf = client.buf in
  let oc  = Output.create ~buf_size:self.buf_size client in
  let is = Input.of_client ~buf_size:self.buf_size client in
  while client.cont do
    try
      match Request.parse_req_start ~client ~buf is with
      | None ->
         Async.stop_client client (* client is done *)

      | Some req ->
       try
         log (Req 1) (fun k->k "req: %s" (Format.asprintf "@[%a@]" Request.pp_ req));

         if Request.close_after_req req then Async.stop_client client;

         (* is there a handler for this path? *)
         let (req, filter, handler) = Route.find self.handlers req in
         (* handle expect/continue *)
         begin match Request.get_header ~f:String.trim req Headers.Expect with
          | Some "100-continue" ->
            log (Req 1) (fun k->k "send back: 100 CONTINUE");
            Response.output_ (Request.meth req) oc (Response.make_raw ~code:continue "");
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
          if Headers.get Headers.Connection r.Response.headers = Some "close" then
            Async.stop_client client;
          Response.output_ (Request.meth req) oc r;
          log (Req 0) (fun k -> k "response code %d sent after %fms" (r.code :> int)
                                    (1e3 *. (Unix.gettimeofday () -. req.start_time)));
        in
        (* call handler *)
        handler oc req ~resp;
        if client.cont then Async.yield ()
      with
      | Headers.Bad_req (c,s,headers,cookies) ->
         log (Req 0) (fun k -> k "not 200 status: %d (%s)" (c :> int) s);
         let res = Response.make_raw ~headers ~cookies ~code:c s in
         begin
           try Response.output_ (Request.meth req) oc res
           with Sys_error _ | Unix.Unix_error _ -> ()
         end;
         if not ((c :> int) < 500) then Async.stop_client client else Async.yield ()
      with
      | Sys_error _ | Unix.Unix_error _
        | Ssl.Write_error _ | Ssl.Read_error _
        | Async.ClosedByHandler | Async.TimeOut as e ->
         log (Exc 1) (fun k -> k "probably broken connection (%s)"
                                    (Async.printexn e));
         Async.stop_client client (* connection broken somehow *)

      | e ->
         log (Exc 0) (fun k -> k "internal server error (%s)"
                                    (Async.printexn e));
         Async.stop_client client
  done;
  client.close ();
  log (Sch 0) (fun k->k "done with client, exiting");
  ()

let save_load (self:t) =
  let _ =
    match self.restart_file with
    | name when name <> "" && Sys.file_exists name ->
       begin
         try
           Log.(f (Exc 0)
                  (fun k -> k "Restoring session from %s\n%!" name));
           let ch = open_in name in
           let rec loop () =
             let name, _ as version = input_value ch in
             if name = Session.save_name then
               begin
                 Session.load_sessions version ch;
                 loop ()
               end
             else if name = Stats.save_name then
               begin
                 Stats.restore version ch;
                 loop()
               end
           in
           loop ();
           close_in ch;
           Sys.remove self.restart_file
         with exn ->
               Printf.eprintf "FATAL ERROR: failed to load %s (%s)\n%!"
                 name (Printexc.to_string exn);
               exit 1
       end
    | _ -> ()
  in
  let quit =
    if self.restart_file = "" then
      fun signal ->
         Log.(f (Exc 0)
                (fun k -> k "Signal %d: closing all clients\n%!" signal));
         Async.close_all signal;
         exit 0;
    else fun signal ->
         Log.(f (Exc 0)
                (fun k -> k "Signal %d: closing all clients\n%!" signal));
         Async.close_all signal;
         Log.(f (Exc 0)
                (fun k -> k "Signal %d: saving session in %s\n%!"
                            signal self.restart_file));
         let ch = open_out_gen [Open_wronly; Open_trunc; Open_creat; Open_binary]
                    0o600 self.restart_file in
         Session.save_sessions ch;
         Stats.save ch;
         output_value ch ("END_SAVE", 0);
         close_out ch;
         exit 0
  in
  let open Sys in
  let quit = Signal_handle quit in
  set_signal sigint quit;
  set_signal sigquit quit;
  set_signal sigterm quit;
  set_signal sigabrt quit

let run (self:t) =
  save_load self ;
  let handler client_sock = handle_client_ self client_sock in
  let maxc = self.max_connections in
  let set_domains ds = self.domains <- ds in
  let a = Async.run ~nb_threads:self.num_threads ~listens:self.listens
        ~maxc ~timeout:self.timeout ~set_domains
        handler
  in
  Array.iter (fun d -> Domain.join d) a
