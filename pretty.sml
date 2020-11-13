infix 5 <>
infix 5 <+>
infix 5 <:>

structure Pretty : sig
  type t = string

  val <> : t * t -> t
  val <+> : t * t -> t
  val <:> : t * t -> t

  val paren : bool -> t -> t
  val brack : t -> t
  val brace : t -> t
end = struct
  type t = string

  fun x <> y = x ^ y
  fun x <+> y = x <> " " <> y
  fun x <:> y = x <+> ":" <+> y

  fun paren true x = "(" <> x <> ")"
    | paren false x = x

  fun brack x = "[" <> x <> "]"

  fun brace x = "{" <> x <> "}"
end
