structure ModVar :> sig
  include ORDERED

  val from_string : string -> t
  val fresh : string -> t

  val show : t -> string
end = struct
  datatype t
    = T of string (* textual *)
    | G of int * string (* generated *)

  val counter = ref 0

  fun fresh hint = G(!counter, hint) before counter := !counter + 1

  fun compare (x, y) =
    case (x, y) of
         (T x, T y) => String.compare (x, y)
       | (G x, G y) => Int.compare (#1 x, #1 y)
       | (T _, G _) => LESS
       | (G _, T _) => GREATER

  fun from_string x = T x

  fun show (T x)     = x
    | show (G(n, x)) = "?" ^ Int.toString n ^ "_" ^ x
end
