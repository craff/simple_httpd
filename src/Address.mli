(** Module for declaring address and port to listen to *)

(** A type to index all listened addresses *)
type index = private int

(** Record type storing the an address we listen on *)
type t = private
  { addr : string  (** The actual address in formal "0.0.0.0" *)
  ; port : int     (** The port *)
  ; ssl  : Ssl.context Atomic.t option (** An optional ssl context *)
  ; reuse : bool   (** Can we reuse the socket *)
  ; mutable index : index (** The index used to refer to the address *)
  }

type ssl_info =
  { protocol : Ssl.protocol
  ; cert : string
  ; priv : string
  }

(** An helper to build the above type *)
val make : ?addr:string -> ?port:int -> ?ssl:ssl_info ->
           ?reuse:bool -> unit -> t

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
