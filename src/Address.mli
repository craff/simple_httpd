(** Module for declaring address and port to listen to *)

(** A type to index all listened addresses *)
type index = private int

(** Record type storing the an address we listen on *)
type t = private
  { addr : string  (** The actual address in formal "0.0.0.0" *)
  ; port : int     (** The port *)
  ; ssl  : Ssl.context option (** An optional ssl context *)
  ; reuse : bool   (** Can we reuse the socket *)
  ; mutable index : index (** The index used to refer to the address *)
  }

(** An helper to build the above type *)
val make : ?addr:string -> ?port:int -> ?ssl:Ssl.context ->
           ?reuse:bool -> unit -> t

(** Internal use *)
val register : (t -> 'a) -> t list -> t array * 'a array
val index : t -> index
