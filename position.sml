structure Position :> sig
  type t

  val initial : t
  val next : char -> t -> t

  val show : t -> string
end = struct
  type t = int * int

  val initial = (1, 1)

  fun newline (l, c) =
    (l + 1, 1)

  fun next c x =
    case c of
         #"\n" => newline x
       | _     => (#1 x, #2 x + 1)

  fun show (l, c) =
    Int.toString l ^ ":" ^ Int.toString c
end

type position = Position.t

type location = { from : position, to : position }

datatype 'a loc = Loc of location * 'a

fun unloc (Loc p) = #2 p
