open Async

val create : string -> string array -> int * Io.t

val wait : ?time:float -> int -> Unix.process_status
