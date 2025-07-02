open Async

type status = Running | Exited of int | Exn of exn

type process = private
  { pid : int
  ; mutable status : status }

val create : ?wait_interval:float ->
             ?stdout:Unix.file_descr -> ?stderr:Unix.file_descr ->
             string -> string array -> process * Io.t
