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
      Semaphore.decr m.sem;
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
      Log.f (Log.Sch lvl)
        (fun g -> f (fun ?(header="") ?(tags=Logs.Tag.empty) ->
                      let sep = if header = "" then "" else ":" in
                      (fun fmt -> g ("(%s%s%s%a)" ^^ fmt)
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
    let wrap_fd fn fd =
      Unix.set_nonblock fd;
      fn (Io.create fd)
    let poll ?(read=false) ?(write=false) ?(timeout= -1.0) _fd =
      let _ = timeout in
      (read, write, not read && not write)

  end

end

include Caqti_connect.Make_unix (System)
