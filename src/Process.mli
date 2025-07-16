open Async

type status = Running | Exited of int | Exn of exn

type process = private
  { pid : int
  ; mutable status : status }

val create : ?wait_interval:float ->
             ?stdout:Unix.file_descr -> ?stderr:Unix.file_descr ->
             client:Client.t -> string -> string array -> process * Io.t

type 'a mail = { dest: string
               ; from: string
               ; subject : string
               ; action : process -> 'a
               ; cmd : string }

val mail : 'a mail -> ('b, Format.formatter, unit, 'a) format4 -> 'b
