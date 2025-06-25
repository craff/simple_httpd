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

module System = struct
  type 'a future = 'a
  let (>>=) x f = f x
  let (>|=) x f = f x
  let return x = x
  let finally f g =
    (match f () with
     | y -> g (); y
     | exception exn -> g (); raise exn)
  let cleanup f g =
    try f () with exn -> g (); raise exn
  let join (_ : unit list) = ()

  module Mvar = struct
    type 'a t = { mut: Mutex.t; sem : Semaphore.t; mutable values : 'a Queue.t }
    let create () = { mut = Mutex.create (); sem = Semaphore.create 0;
                      values = Queue.create () }
    let store x m =
      Mutex.lock m.mut;
      Queue.add x m.values;
      Semaphore.incr m.sem;
      Mutex.unlock m.mut

    let fetch m =
      Mutex.lock m.mut;
      Semaphore.decr m.mut m.sem;
      let r = Queue.peek m.values in
      Mutex.unlock m.mut;
      r

  end

  module Stream = Caqti_stream.Make (struct
    type 'a future = 'a
    let (>>=) x f = f x
    let (>|=) x f = f x
    let return x = x
  end)

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

  module Preemptive = struct
    let detach f x = f x
    let run_in_main f = f ()
  end

  module Unix = struct
    type file_descr = Io.t
    let tbl = Array.init Simple_httpd__Util.maxfd (fun _ -> Atomic.make None)
    let wrap_fd fn fd =
      let nfd = Simple_httpd__Util.file_descr_to_int fd in
      let rec iofd ()  =
        match Atomic.get tbl.(nfd) with
        | Some io -> io
        | None ->
           Unix.set_nonblock fd;
           let finalise _ =
             Atomic.set tbl.(nfd) None;
             Unix.close fd;
           in
           let res = Io.create ~finalise fd in
           if Atomic.compare_and_set tbl.(nfd) None (Some res) then
             res
           else
             begin
               Io.close res;
               iofd ()
             end
      in
      fn (iofd ())

    let poll ?(read=false) ?(write=false) ?(timeout= -1.0) (_fd:file_descr) =
      let _ = timeout in
      (read, write, not read && not write)

  end

end

include Caqti_connect.Make_unix (System)

(* level is managed by simple_httpd, put maximum for caqti *)
let _ = Logs.(set_level ~all:true (Some Debug))

let cleanup_no_client (m, (conn : connection)) =
  Log.(f (Sch 0) (fun k -> k "cleanup"));
  let module C : CONNECTION = (val conn) in
  (try C.disconnect () with _ -> ());
  Mutex.delete m;
  false

let db_key = Session.new_key ~cleanup_no_client "Caqti_db"

let create_connection db_config req =
  let conn =
    match connect db_config with
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

let get_connection db_config req =
  match Session.get_session req with
  | None -> create_connection db_config req
  | Some session ->
     match Session.get_session_data session db_key with
     | None -> create_connection db_config req
     | Some r -> (r, true)

let with_session ~db_config req f =
  let ((m, db as dbt), with_session) = get_connection db_config req in
  Mutex.lock m;
  try
    let res = f db in
    Mutex.unlock m;
    if not with_session then ignore (cleanup_no_client dbt);
    res
  with exn -> Mutex.unlock m; raise exn
