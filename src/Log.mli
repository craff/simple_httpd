
val set_log_lvl : int -> unit

val set_log_folder : ?basename:string -> ?perm:int -> string -> int -> unit

val f : ?lvl:int ->
        ((('a, out_channel, unit, unit) format4 -> 'a) -> unit) -> unit
