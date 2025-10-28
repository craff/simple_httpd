(* Copyright (C) 2014--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Connecting with Lwt.

    This module contains the signature and connect function specialized for use
    with Lwt. *)

module Stream : Caqti_stream_sig.S with type 'a fiber := 'a
module Pool : Caqti_pool_sig.S with type 'a fiber := 'a

module type CONNECTION = Caqti_connection_sig.S
  with type 'a fiber := 'a
   and type ('a, 'e) stream := ('a, 'e) Stream.t

include Caqti_connect_sig.S
  with type 'a fiber := 'a
   and type 'a with_switch := 'a
   and type 'a with_stdenv := 'a
   and type ('a, 'e) stream := ('a, 'e) Stream.t
   and type ('a, 'e) pool := ('a, 'e) Pool.t
   and type connection = (module CONNECTION)

(** [with_session] will attach a connection to the request's session, or reuse
    a previous connection. The data base handle will be closed when the client
    disconnects all its connection (the timeout is important here).

    The advantage of this approach is that you can use a role that depends on
    the user session (with permission adapted to the user).

    You can use
      [with_connection : config:Caqti_connect_config.t ->
         Uri.t -> (connection -> 'b) -> 'b]
    if you want to connect and disconnect for that session or other feature
    proposed by caqti like pool of connection.  *)
val with_session : config:Caqti_connect_config.t -> Uri.t
                   -> 'a Simple_httpd.Request.t -> (connection -> 'b) -> 'b
