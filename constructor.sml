structure Constructor :> sig
  type t

  val from_string : string -> t
  val show : t -> string

  val compare : t * t -> order
end = struct
  type t = string

  fun from_string s = s

  fun show s = s

  val compare = String.compare
end

type con = Constructor.t

(* `Sum` represents labeled sums. *)
structure Sum = BinarySearchMap (Constructor)
