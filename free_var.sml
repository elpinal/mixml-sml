structure FVar :> sig
  type t

  val get_kind : t -> kind
  val fresh : kind -> t
  val equal : t -> t -> bool
  val show : t -> string

  structure Map : MAP where type key = t
end = struct
  type t = int * kind

  val counter = ref 0

  fun get_kind (_, k) = k

  fun fresh k = (!counter, k) before counter := !counter + 1

  fun equal (x, _) (y, _) = x = y

  fun show (n, _) = "!" ^ Int.toString n

  structure Map = BinarySearchMap (struct
    type t = t
    fun compare ((x, _), (y, _)) = Int.compare (x, y)
  end)
end

type fvar = FVar.t
