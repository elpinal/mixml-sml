structure Label :> sig
  eqtype t

  val compare : t * t -> order

  val from_string : string -> t

  val ann : t -> string -> t

  val show : t -> string
end = struct
  type t = string

  val compare = String.compare

  fun from_string x = x

  fun ann x y = x ^ "_" ^ y

  fun show x = x
end
