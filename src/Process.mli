open Async

val create : ?wait_interval:float -> string -> string array -> int * Io.t
