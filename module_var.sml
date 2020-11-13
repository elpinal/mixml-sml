structure ModVar :> sig
  include ORDERED

  val from_string : string -> t

  val show : t -> string
end = struct
  type t = string

  val compare = String.compare

  fun from_string x = x

  fun show x = x
end
