(** Module for declaring address and port to listen to *)

(** A type to index all listened addresses *)
type index = private int

(** Record type storing the an address we listen on *)
type t = private
  { addr : string  (** The actual address in formal "0.0.0.0" *)
  ; port : int     (** The port *)
  ; hosts: string list (** The host we accept: any if the list is empty,
                           only those listed otherwise *)
  ; ssl  : Ssl.context Atomic.t option (** An optional ssl context *)
  ; reuse : bool   (** Can we reuse the socket *)
  ; mutable index : index ref (** The index used to refer to the address, shared by
                                  all addresses with the same IP and port*)
  }

type ssl_info =
  { protocol : Ssl.protocol (** min protocol version, max is 1_3 *)
  ; cert : string
  ; priv : string
  }

(** An helper to build the above type *)
val make : ?addr:string -> ?port:int -> ?hosts:string list -> ?ssl:ssl_info ->
           ?reuse:bool -> unit -> t

(** Functions to reuse the same address and ssl certificate with a different
    port or hosts. *)
val change_hosts : string list -> t -> t
val change_port  : int -> t -> t

(** set the period in second at which all ssl certificates are checked for
    renewal *)
val set_ssl_reload_period : int -> unit

(** Internal use *)
val register : (t -> 'a) -> t list -> t array * 'a array
val index : t -> index
val forward_log
    : ((((string -> string -> string -> unit, out_channel, unit, unit) format4 ->
         string -> string -> string -> unit) -> unit) -> unit) ref
val dummy : t
val set_index_ref : t -> index ref -> unit
