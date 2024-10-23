type 'a key
type data

val new_key : 'a. ('a -> bool) -> ('a -> unit) ->
              (out_channel -> 'a -> unit) ->
              (in_channel -> 'a) ->
              string -> 'a key
val search : 'a key -> data -> 'a
val add_replace : 'a key -> 'a -> data -> data
val remove : 'a key -> data -> data
val cleanup_delete : data -> unit
val cleanup_filter : data -> data
val cleanup : 'a key -> 'a -> bool
val empty : data

type key_ref = R : 'a key -> key_ref

(* Where loading session only *)
val get_key : string -> key_ref
val get_idx : 'a key -> string
val save : out_channel -> data -> unit
val load : in_channel -> data
