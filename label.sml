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

type label = Label.t

structure Record = BinarySearchMap(Label)

structure Path :> sig
  type t

  val empty : t
  val is_empty : t -> bool

  val extend : t -> label -> t
  val append : t -> label list -> t
  val prepend : label -> t -> t

  val uncons : t -> (label * t) option

  val to_list : t -> label list

  val start_with : t -> t -> bool

  val show : t -> string
end = struct
  type t = label Deque.t

  val empty = Deque.empty

  val is_empty = Deque.is_empty

  val extend = Deque.push

  fun append xs =
  let fun f acc [] = acc
        | f acc (y :: ys) = f (Deque.push acc y) ys
  in
    f xs
  end

  val prepend = Deque.cons

  fun uncons xs =
    case Deque.head xs of
         SOME x => SOME (x, Deque.tail xs)
       | NONE   => NONE

  val to_list = Deque.to_list

  fun start_with prefix p =
    case (uncons p, uncons prefix) of
         (NONE, _) => is_empty prefix
       | (SOME(l1, p'), SOME(l2, prefix')) =>
           if l1 = l2
           then start_with prefix' p'
           else false
       | (SOME _, NONE) => true

  fun show_list' x [] = x
    | show_list' x (y :: ys) = x ^ "." ^ show_list' y ys

  fun show_list [] = ""
    | show_list (x :: xs) = show_list' x xs

  fun show p = show_list (map Label.show (Deque.to_list p))
end

type path = Path.t
