
(** Module type for filling an input buffer by printing. A module of
    this type is openned in [<ML>] section of [.chamel] file when
    using [vfs_pack]. *)
module type Output = sig
  val echo : string -> unit
  val printf : ('a, Format.formatter, unit, unit) format4 -> 'a
end

type elt = (module Output) -> unit

type chaml = string Request.t -> Headers.t -> Headers.t * Cookies.t * Input.t
