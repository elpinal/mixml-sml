datatype kind
  = KBase
  | KArrow of kind * kind

structure Kind : sig
  type t = kind

  val show : t -> string
end = struct
  open Pretty
  open Std

  type t = kind

  fun show _ KBase          = "Type"
    | show n (KArrow(x, y)) = paren (n > 2) $ show 3 x <+> "->" <+> show 2 y

  val show = show 0
end
