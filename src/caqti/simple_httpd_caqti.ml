(* Copyright (C) 2014--2021  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

open Simple_httpd

type Caqti_error.msg += Msg_unix of Unix.error * string * string

module Fiber = struct
  type +'a t = 'a
  module Infix = struct
    let (>>=) x f = f x
    let (>|=) x f = f x
  end

  let return x = x
  let catch f g = try f () with exn -> g exn
  let finally f g =
    (match f () with
     | y -> g (); y
     | exception exn -> g (); raise exn)
  let cleanup f g =
    try f () with exn -> g (); raise exn
end

module Stream = Caqti_platform.Stream.Make (Fiber)
module Switch = Caqti_platform.Switch.Make (Fiber)

module System = struct
  type stdenv = unit

  module Fiber = Fiber
  module Stream = Stream
  module Switch = Switch

  let async ~sw:_ fn =
    match Async.spawn fn () with
    | Ok x -> x
    | Error exn -> raise exn

  module Mutex = Simple_httpd.Mutex

  module Condition = struct
    type t = Simple_httpd.Semaphore.t
    let create () = Simple_httpd.Semaphore.create 1
    let wait a b = Simple_httpd.Semaphore.decr b a
    let signal = Simple_httpd.Semaphore.incr
  end

  module Sequencer = struct
    type 'a t = 'a
    let create x = x
    let enqueue x f = f x
  end

  module Log = struct
    type 'a log = ('a, unit) Logs.msgf -> unit
    let gen lvl ?(src=Logs.default) (f: ('a,unit) Logs.msgf) =
      let src = Logs.Src.name src in
      Log.f (Log.Req lvl)
        (fun g -> f (fun ?(header="") ?(tags=Logs.Tag.empty) ->
                      let sep = if header = "" then "" else ":" in
                      (fun fmt -> g ("(%s%s%s%a) " ^^ fmt)
                                    src sep header Logs.Tag.pp_set tags)))
    let err ?src f = gen 0 ?src f
    let warn ?src f = gen 1 ?src f
    let info ?src f = gen 2 ?src f
    let debug ?src f = gen 3 ?src f
  end

  module Net = struct

    module Sockaddr = struct
      type t = Unix.sockaddr
      let unix s = Unix.ADDR_UNIX s
      let tcp (addr, port) =
        Unix.ADDR_INET (Unix.inet_addr_of_string (Ipaddr.to_string addr), port)
    end

    let getaddrinfo ~stdenv:() host port =
      try
        let opts = Unix.[AI_SOCKTYPE SOCK_STREAM] in
        Unix.getaddrinfo (Domain_name.to_string host) (string_of_int port) opts
        |> List.map (fun ai -> ai.Unix.ai_addr) |> Result.ok
      with
      | Not_found -> Ok []
      | Unix.Unix_error (code, _, _) ->
         Error (`Msg ("Cannot resolve host name: " ^ Unix.error_message code))

    let convert_io_exception = function
      | Unix.Unix_error (err, fn, arg) -> Some (Msg_unix (err, fn, arg))
      | _ -> None

    module Socket = struct
      type t = { infd: Io.t; inp: Input.t;
                 outfd: Io.t; fmt : Format.formatter }

      let output_char {fmt; _} c =
        Format.fprintf fmt "%c" c
      let output_string {fmt; _} s =
        Format.fprintf fmt "%s" s
      let flush {fmt; _} = Format.fprintf fmt "%!"

      let input_char {inp; _} =
        Input.read_char inp

      let really_input {inp; _} buf i n =
        Input.really_input inp buf i n

      let close {infd; outfd; _ } =
        Io.close infd; Io.close outfd
    end

    type tcp_flow = Socket.t
    type tls_flow = Socket.t

    let connect_tcp ~sw:_ ~stdenv:() sockaddr =
      try
        let open Socket in
        let ic, oc = Unix.open_connection sockaddr in
        let infd = Io.create (Unix.descr_of_in_channel ic) in
        let outfd = Io.create (Unix.descr_of_out_channel oc) in
        let inp = Input.of_io infd in
        let fmt = Io.formatter outfd in
        Ok { infd; outfd; inp; fmt }
      with
       | Unix.Unix_error (err, func, arg) -> Error (Msg_unix (err, func, arg))

    let tcp_flow_of_socket _ = None
    let socket_of_tls_flow ~sw:_ = Fun.id

    module type TLS_PROVIDER = Caqti_platform.System_sig.TLS_PROVIDER
      with type 'a fiber := 'a
       and type tcp_flow := Socket.t
       and type tls_flow := Socket.t

    let tls_providers_r : (module TLS_PROVIDER) list ref = ref []

    let register_tls_provider p = tls_providers_r := p :: !tls_providers_r

    let tls_providers _ =
      (* Try to load caqti-tls.unix here if/when implemented. *)
      !tls_providers_r
  end

end

module Pool = Caqti_platform.Pool.Make_without_alarm (System)

module System_unix = struct
  module Preemptive = struct
    let detach f x = f x
    let run_in_main f = f ()
  end

  module Unix = struct
    type file_descr = Unix.file_descr

    let wrap_fd fn fd = fn fd

    let poll ~stdenv:() ?(read=false) ?(write=false)
          ?(timeout= -1.0) (fd:file_descr) =
      let _ = timeout in
      let evt = Polly.Events.oneshot in
      let evt = if read then Polly.Events.(inp lor rdhup lor evt)
                else if write then Polly.Events.(out lor evt)
                else evt
      in
      Async.schedule_fd_once evt read fd;
      if read then (read, false, false)
      else (false, write, false)
  end
end

module Loader = Caqti_platform_unix.Driver_loader.Make (System) (System_unix)

include Caqti_platform.Connector.Make (System) (Pool) (Loader)

module type CONNECTION = Loader.CONNECTION

type connection = (module CONNECTION)

let connect ?subst ?env ?config ?tweaks_version uri =
  let sw = Switch.create () in
  connect ?subst ?env ?config ?tweaks_version ~sw ~stdenv:() uri

(* level is managed by simple_httpd, put maximum for caqti *)
let _ = Logs.(set_level ~all:true (Some Debug))

let cleanup_no_client (m, (conn : connection)) =
  Log.(f (Sch 0) (fun k -> k "cleanup"));
  let module C : CONNECTION = (val conn) in
  (try C.disconnect () with _ -> ());
  Mutex.delete m;
  false

let db_key = Session.new_key ~cleanup_no_client "Caqti_db"

let create_connection ~config uri req =
  let conn =
    match connect ~config uri with
    | Ok conn -> conn
    | Error err -> raise (Caqti_error.Exn err)
  in
  let mutex = Mutex.create () in
  let res = (mutex, conn) in
  Gc.finalise (fun db -> ignore (cleanup_no_client db)) res;
  let with_session =
    match Session.get_session req with
    | None -> false
    | Some session ->
       Session.set_session_data session db_key res;
        true
  in
  (res, with_session)

let get_connection ~config uri req =
  match Session.get_session req with
  | None -> create_connection ~config uri req
  | Some session ->
     match Session.get_session_data session db_key with
     | None -> create_connection ~config uri req
     | Some r -> (r, true)

let with_session ~config uri req f =
  let ((m, db as dbt), with_session) = get_connection ~config uri req in
  Mutex.lock m;
  try
    let res = f db in
    Mutex.unlock m;
    if not with_session then ignore (cleanup_no_client dbt);
    res
  with exn ->
    Mutex.unlock m; raise exn

let with_connection = with_connection ~stdenv:()

let connect_pool
      ?pool_config ?post_connect ?subst ?env ?config ?tweaks_version uri =
  let sw = Switch.create () in
  connect_pool
    ?pool_config ?post_connect ?subst ?env ?config ?tweaks_version
    ~sw ~stdenv:() uri
