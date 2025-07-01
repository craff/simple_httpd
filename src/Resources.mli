module type Resource = sig
  type t
  val create : unit -> t
  val number : int
end

module type Resources = sig
  type t
  type handle
  val get : unit -> handle * t
  val release : handle -> unit
  val use : (t -> 'a) -> 'a
end

module Make(R:Resource) : Resources with type t = R.t
