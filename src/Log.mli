val set_log_requests        : int -> unit
val set_log_scheduler       : int -> unit
val set_log_exceptions      : int -> unit
val set_log_authentications : int -> unit
val set_log_processes       : int -> unit
val set_log_user            : int -> unit

val set_log_folder : ?basename:string -> ?perm:int -> string -> int -> unit

type log_lvl =
  | Req of int
  | Sch of int
  | Exc of int
  | Aut of int
  | Prc of int
  | Usr of int

val f : log_lvl ->
        ((('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) -> unit

val log_folder : string ref
val log_basename : string ref
val log_perm : int ref

val fname : int -> string

val init_log_folder : int -> unit
