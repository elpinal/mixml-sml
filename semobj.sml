structure L : sig
  val index : fvar -> fvar list -> int option
end = struct
  fun index v =
  let fun go i =
    fn [] => NONE
     | x :: xs =>
         if FVar.equal x v
         then SOME i
         else go (i + 1) xs
  in
    go 0
  end
end

signature S = sig
  eqtype label
  eqtype base
  type con

  val show_label : label -> string
  val show_base : base -> string
  val show_con : con -> string

  structure Record : MAP where type key = label
  structure Sum : MAP where type key = con
end

functor SemObj (X : S) = struct
  open X

  datatype kind = datatype kind

  datatype ty
    = TBound of int * int
    | TFree of fvar
    | TAbs of kind * ty
    | TApp of ty * ty
    | TArrow of ty * ty
    | TBase of base
    | TSum of ty Sum.t
    | TTuple of ty list
    | TBottom

  datatype polarity
    = Import
    | Export

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

    fun show p = show_list (map show_label (Deque.to_list p))
  end

  type path = Path.t

  datatype modsig
    = Type of ty
    | Val of ty * polarity
    | Unit of unitsig
    | Str of modsig Record.t

  withtype unitsig
    = (kind * path) list * kind list * modsig

  datatype realizer
    = RAtom of ty
    | RStr of realizer Record.t

  val rec abs =
    fn Type ty           => Type ty
     | Val(ty, _)        => Val(ty, Export)
     | Unit(u : unitsig) => Unit u
     | Str r             => Str (Record.map abs r)

  fun open_at j tys =
  let
    fun go c =
      fn TBound(i, j) =>
           if c = i
           then List.nth (tys, j)
           else TBound(i, j)
       | TFree v => TFree v
       | TAbs(k, x) => TAbs(k, go (c + 1) x)
       | TApp(x, y) => TApp(go c x, go c y)
       | TArrow(x, y) => TArrow(go c x, go c y)
       | TBase b      => TBase b
       | TSum s       => TSum(Sum.map (go c) s)
       | TTuple xs    => TTuple(map (go c) xs)
       | TBottom      => TBottom
  in
    go j
  end

  fun open_at_modsig j tys =
    fn Type ty    => Type (open_at j tys ty)
     | Val(ty, p) => Val(open_at j tys ty, p)
     | Unit u     => Unit (open_at_unitsig j tys u)
     | Str r      => Str (Record.map (open_at_modsig j tys) r)

  and open_at_unitsig j tys (is, es, s) =
    (is, es, open_at_modsig (j + 2) tys s)

  fun close_at j fvs =
  let
    fun go c =
      fn TBound x => TBound x
       | TFree v  =>
           let in
             case L.index v fvs of
                  SOME n => TBound(c, n)
                | NONE   => TFree v
           end
       | TAbs(k, x) => TAbs(k, go (c + 1) x)
       | TApp(x, y) => TApp(go c x, go c y)
       | TArrow(x, y) => TArrow(go c x, go c y)
       | TBase b      => TBase b
       | TSum s       => TSum(Sum.map (go c) s)
       | TTuple xs    => TTuple(map (go c) xs)
       | TBottom      => TBottom
  in
    go j
  end

  fun close_at_modsig j fvs =
    fn Type ty    => Type (close_at j fvs ty)
     | Val(ty, p) => Val(close_at j fvs ty, p)
     | Unit u     => Unit (close_at_unitsig j fvs u)
     | Str r      => Str (Record.map (close_at_modsig j fvs) r)

  and close_at_unitsig j fvs (is, es, s) =
    (is, es, close_at_modsig (j + 2) fvs s)

  structure Show : sig
    val show_type : int -> ty -> string
    val show_modsig : modsig -> string

    val show_list : string list -> string
  end = struct
    open Pretty
    open Std

    fun show_list' x [] = x
      | show_list' x (y :: ys) = x <> "," <+> show_list' y ys

    fun show_list [] = ""
      | show_list (x :: xs) = show_list' x xs

    fun show_list_with' _ x [] = x
      | show_list_with' s x (y :: ys) = x <> s <> show_list_with' s y ys

    fun show_list_with _ [] = ""
      | show_list_with s (x :: xs) = show_list_with' s x xs

    fun show_type n =
      fn TBound _     => raise Unreachable
       | TFree v      => FVar.show v
       | TApp(x, y)   => paren (n > 4) $ show_type 4 x <+> show_type 5 y
       | TArrow(x, y) => paren (n > 2) $ show_type 3 x <+> "->" <+> show_type 2 y
       | TBase b      => show_base b
       | TSum s       => brack $ show_list $ map (fn (c, s) => show_con c <:> s) $
           Sum.to_list $ Sum.map (show_type 0) s
       | TTuple xs    => paren (n > 3) $ show_list_with " * " $ map (show_type 4) xs
       | TBottom      => "bottom"
       | TAbs(k, x)   =>
           let val fv = FVar.fresh k in
             paren (n > 0) $
               "fun" <+> paren true (FVar.show fv <:> Kind.show k) <+> "->"
                 <+> show_type 0 (open_at 0 [TFree fv] x)
           end

    fun show_polarity Export = "+"
      | show_polarity Import = "-"

    fun show_modsig s =
      case s of
           Type ty => brack $ "=" <+> show_type 0 ty
         | Val(ty, p) => brack (show_type 0 ty) <> show_polarity p
         | Unit u => brack (show_unit u) <> show_polarity Export
         | Str r => brace $ show_list $
             map (fn (l, s) => show_label l <:> s) $ Record.to_list $ Record.map show_modsig r

    and show_unit (is, es, s) =
    let
      val ifvs = map (fn (k, _) => FVar.fresh k) is
      val efvs = map FVar.fresh es

      fun i acc =
        if null is
        then acc
        else
          "forall" <+>
          show_list
            (map (fn (v, (k, p)) => paren true $ FVar.show v <> "@" <> Path.show p <:> Kind.show k) (ListPair.zipEq (ifvs, is)))
          <> "." <+> acc

      fun e acc =
        if null es
        then acc
        else
          "exist" <+>
          show_list
            (map (fn (v, k) => paren true $ FVar.show v <:> Kind.show k) (ListPair.zipEq (efvs, es)))
          <> "." <+> acc
    in
      i $ e $ show_modsig $ open_at_modsig 0 (map TFree efvs) $ open_at_modsig 1 (map TFree ifvs) s
    end
  end

  fun reduce (TApp(x, y)) = reduce' (reduce x) y
    | reduce ty           = ty

  and reduce' (TAbs(_, x)) y = reduce (open_at 0 [y] x)
    | reduce' x y            = TApp(x, y)

  exception VarMismatch of fvar * fvar
  exception TypeMismatch of ty * ty
  exception BaseMismatch of base * base
  exception NotArrowKind of kind
  exception OnlyInLeft of con * ty * ty
  exception OnlyInRight of con * ty * ty

  (* beta eta equivalence *)
  fun equal_type x y k : unit =
    case k of
         KArrow(k1, k2) =>
           let val fv = TFree (FVar.fresh k1) in
             equal_type (TApp(x, fv)) (TApp(y, fv)) k2
           end
       | KBase => ignore (str_equiv (reduce x) (reduce y))

  and str_equiv ty1 ty2 : kind =
    case (ty1, ty2) of
         (TArrow(x1, y1), TArrow(x2, y2)) =>
           KBase
           before equal_type x1 x2 KBase
           before equal_type y1 y2 KBase
       | (TFree v1, TFree v2) =>
           if FVar.equal v1 v2
           then FVar.get_kind v1
           else raise VarMismatch(v1, v2)
       | (TApp(x1, y1), TApp(x2, y2)) =>
           let val k = str_equiv x1 x2 in
             case k of
                  KBase => raise NotArrowKind k
                | KArrow(k1, k2) => k2 before equal_type y1 y2 k1
           end
       | (TBase b1, TBase b2) => if b1 = b2 then KBase else raise BaseMismatch(b1, b2)
       | (TSum s1, TSum s2)   =>
           let
             fun f () c ty1 =
               case Sum.lookup c s2 of
                    SOME ty2 => equal_type ty1 ty2 KBase
                  | NONE     => raise OnlyInLeft(c, TSum s1, TSum s2)

             fun g () c _ =
               case Sum.lookup c s2 of
                    SOME _ => ()
                  | NONE   => raise OnlyInRight(c, TSum s1, TSum s2)
           in
             Sum.fold_left f () s1;
             Sum.fold_left g () s2;
             KBase
           end
       | (TTuple xs, TTuple ys) =>
           let in
             ListPair.appEq (fn (x, y) => equal_type x y KBase) (xs, ys)
             handle ListPair.UnequalLengths => raise TypeMismatch(ty1, ty2)
             ; KBase
           end
       | (TBottom, TBottom)   => KBase
       | _ => raise TypeMismatch(ty1, ty2)

  exception CannotRealize of path

  fun lookup_realizer (p : path) : realizer -> ty =
    fn RAtom ty =>
         if Path.is_empty p
         then ty
         else raise CannotRealize p
     | RStr r =>
         case Path.uncons p of
              NONE => raise CannotRealize p
            | SOME(l, p') =>
                lookup_realizer p' (valOf (Record.lookup l r) handle Option => raise CannotRealize p)

  exception NotLocallyClosed
  exception KindMismatch of kind * kind

  fun kind_of kks =
    fn TBound(i, j) => List.nth (List.nth (kks, i), j)
     | TFree fv     => FVar.get_kind fv
     | TAbs(k, x)   => KArrow(k, kind_of ([k] :: kks) x)
     | TApp(x, y)   =>
         let
           val k1 = kind_of kks x
           val k2 = kind_of kks y
         in
           case k1 of
                KArrow(k11, k12) =>
                  if k11 = k2
                  then k12
                  else raise KindMismatch(k11, k2)
              | KBase => raise NotArrowKind(k1)
         end
     | TArrow(x, y) =>
         KBase
         before kindcheck kks x KBase
         before kindcheck kks y KBase
     | TBase _ => KBase
     | TSum s => KBase before Sum.app (fn ty => kindcheck kks ty KBase) s
     | TTuple xs => KBase before app (fn ty => kindcheck kks ty KBase) xs
     | TBottom => KBase

  and kindcheck kks ty k =
  let val k' = kind_of kks ty in
    if k' = k
    then ()
    else raise KindMismatch(k', k)
  end

  val kind_of =
    fn ty =>
      kind_of [] ty
      handle Subscript => raise NotLocallyClosed

  val kindcheck =
    fn ty => fn k =>
      kindcheck [] ty k
      handle Subscript => raise NotLocallyClosed

  val rec free_vars =
    fn TBound _ => raise NotLocallyClosed
     | TFree fv => FVar.Map.singleton fv ()
     | TAbs(k, x) =>
         let val fv = FVar.fresh k in
           FVar.Map.delete fv (free_vars (reduce (open_at 0 [TFree fv] x)))
         end
     | TApp(x, y) => FVar.Map.union (free_vars x) (free_vars (reduce y)) (* We don't need `reduce` at head position. *)
     | TArrow(x, y) => FVar.Map.union (free_vars (reduce x)) (free_vars (reduce y))
     | TBase _ => FVar.Map.empty
     | TSum s =>
         Sum.fold_left
           (fn acc => fn _ => fn ty => FVar.Map.union acc (free_vars ty))
           FVar.Map.empty
           s
     | TTuple xs =>
         foldl
           (fn (ty, acc) => FVar.Map.union acc (free_vars ty))
           FVar.Map.empty
           xs
     | TBottom => FVar.Map.empty

  val free_vars : ty -> unit FVar.Map.t = free_vars o reduce

  fun free_vars_modsig s =
    case s of
         Type ty => free_vars ty
       | Val(ty, _) => free_vars ty
       | Unit u     => free_vars_unit u
       | Str r      =>
           Record.fold_left
             (fn acc => fn _ => fn s => FVar.Map.union acc (free_vars_modsig s))
             FVar.Map.empty
             r

  and free_vars_unit (is, es, s) =
  let
    val ifvs = map (fn (k, _) => TFree (FVar.fresh k)) is
    val efvs = map (TFree o FVar.fresh) es
    val s' = open_at_modsig 0 efvs (open_at_modsig 1 ifvs s)
  in
    free_vars_modsig s'
  end

  exception NotAbsolute of modsig
  exception MergeValExports of ty * ty

  fun must_be_absolute (s : modsig) : unit =
    case s of
         Type _ => ()
       | Val(_, p) =>
           if p = Export
           then ()
           else raise NotAbsolute(s)
       | Unit _ => ()
       | Str r => Record.app must_be_absolute r

  exception MergeError of modsig * modsig

  (* We don't have explicit subtyping and implicit polymorphism. *)
  fun subtype x y =
  let
    val x = reduce x
  in
    case x of
         TBottom => ()
       | _       => equal_type x y KBase
  end

  fun merge_polarity Import Import = Import
    | merge_polarity _ _           = Export

  fun merge s1 s2 : modsig =
    case (s1, s2) of
         (Type x, Type y) =>
           let
             val k = kind_of x
             val () = kindcheck y k
             val () = equal_type x y k
           in
             Type x
           end
       | (Val(x, Import), Val(y, p2)) => Val(y, p2) before subtype y x
       | (Val(x, p1), Val(y, Import)) => Val(x, p1) before subtype x y
       | (Val(x, Export), Val(y, Export)) => raise MergeValExports(x, y)
       | (Str r1, Str r2) => Str (Record.union_with merge r1 r2)
       | (Str r, s) =>
           if Record.is_empty r
           then s
           else raise MergeError (Str r, s)
       | (s, Str r) =>
           if Record.is_empty r
           then s
           else raise MergeError (Str r, s)
       | _ => raise MergeError (s1, s2)

  structure Subst :> sig
    type t

    val id : t
    val cons : fvar * ty -> t -> t

    val apply : t -> ty -> ty
    val apply_modsig : t -> modsig -> modsig
    val apply_realizer : t -> realizer -> realizer

    val show : t -> string
  end = struct
    type t = (fvar * ty) list

    val id = []

    fun cons p xs = p :: xs

    fun apply (xs : t) ty =
      open_at 0 (map #2 xs) (close_at 0 (map #1 xs) ty)

    fun apply_modsig (subst : t) =
      fn Type ty => Type (apply subst ty)
       | Val(ty, p) => Val(apply subst ty, p)
       | Unit u => Unit (apply_unit subst u)
       | Str r  => Str (Record.map (apply_modsig subst) r)

    and apply_unit subst (is, es, s) =
      (is, es, apply_modsig subst s)

    fun apply_realizer subst =
      fn RAtom ty => RAtom (apply subst ty)
       | RStr r   => RStr (Record.map (apply_realizer subst) r)

    fun show xs =
    let
      open Show
      open Std
      open Pretty
    in
      show_list $ map (fn (v, ty) => FVar.show v <:> show_type 0 ty) xs
    end
  end

  exception NotStructure of modsig
  exception NotTypeComponent of modsig
  exception MissingLabel of label
  exception TypeSelfCycle of path
  exception TypeCycle of path * path

  local
    structure C = Cycle (struct type t = fvar end)
    type locator = (fvar * path) list

    fun lookup p s =
      case Path.uncons p of
           NONE => s
         | SOME(l, p') =>
             case s of
                  Str r => lookup p' (valOf (Record.lookup l r) handle Option => raise MissingLabel l)
                | _     => raise NotStructure s

    fun lookup_type p s =
      case lookup p s of
           Type ty => ty
         | s'      => raise NotTypeComponent s'

    fun find (loc : locator) (v : fvar) : path =
      case loc of
           [] => raise Std.Unreachable
         | (v', p) :: loc =>
             if FVar.equal v v'
             then p
             else find loc v
  in
    fun bidirectional_lookup (loc1 : locator, s1) (loc2 : locator, s2) : Subst.t =
    let
      val xs : (fvar * ty) list =
        map (fn (fv, p) => (fv, lookup_type p s2)) loc1
        @
        map (fn (fv, p) => (fv, lookup_type p s1)) loc2
      val fvs = map #1 xs
      val (g, vs) = C.make_vertices fvs

      val rel = FVar.Map.from_list (ListPair.zipEq (fvs, vs))
      fun getv fv = FVar.Map.lookup fv rel

      fun f (v : C.vertex, (_, ty)) =
      let
        val deps : unit FVar.Map.t = free_vars ty
        fun add_edge d () =
          case getv d of
               SOME w => C.add_edge g v w
             | NONE   => () (* Ignore an unrelated free type variable. *)
      in
        FVar.Map.app_with_key add_edge deps
      end handle
          C.SelfRef(v, _) => raise TypeSelfCycle(find (loc1 @ loc2) (C.from_vertex g v))
        | C.Cycle(v, w) => raise
            TypeCycle(find (loc1 @ loc2) (C.from_vertex g v), find (loc1 @ loc2) (C.from_vertex g w))

      val () = app f (ListPair.zipEq (vs, xs))

      val m = FVar.Map.from_list xs
      fun get_type fv = valOf (FVar.Map.lookup fv m)
      val sfvs = rev (C.sort g)
    in
      foldl (fn (fv, acc) => Subst.cons (fv, Subst.apply acc (get_type fv)) acc) Subst.id sfvs
    end
  end

  structure Template = struct
    datatype loctemp
      = LAtom of kind
      | LStr of loctemp Record.t

    datatype modtemp
      = Type of kind
      | Unit of unittemp
      | Str of modtemp Record.t

    withtype unittemp = loctemp * kind list * modtemp

    exception NotStructureLocator of loctemp

    val rec show_loctemp =
      fn LAtom k => Kind.show k
       | LStr r  =>
           let open Pretty in
             (brace o Show.show_list o map (fn (l, s) => show_label l ^ ":" ^ s) o Record.to_list o Record.map show_loctemp) r
           end

    (* Return an empty structure if missing. *)
    fun lookup_loctemp l =
      fn LStr r => (valOf (Record.lookup l r) handle Option => LStr Record.empty)
       | l      => raise NotStructureLocator l

    fun filter_loctemp (ps : path list) : loctemp -> loctemp =
      fn LAtom k => if List.exists Path.is_empty ps then LAtom k else LStr Record.empty
       | LStr r =>
           let
             fun f acc l loc =
             let val ps' =
               foldl (fn (p, xs) => case Path.uncons p of NONE => xs | SOME(l', p') =>
                      if l = l'
                      then p' :: xs
                      else xs) [] ps
             in
               if null ps'
               then acc
               else Record.insert l (filter_loctemp ps' loc) acc
             end
           in
             LStr (Record.fold_left f Record.empty r)
           end

    fun get_kinds (LAtom k) = [k]
      | get_kinds (LStr r)  =
          let fun f acc _ loc =
            acc @ get_kinds loc
          in
            Record.fold_left f [] r
          end

    fun dom_loctemp (LAtom _) = [Path.empty]
      | dom_loctemp (LStr r) =
          let
            fun f (acc : path list) l loc =
              map (Path.prepend l) (dom_loctemp loc)
              @
              acc
          in
            Record.fold_left f [] r
          end

    fun split (x : loctemp) (y : loctemp) =
    let
      val d1 = dom_loctemp x
      val d2 = dom_loctemp y
      val x_ref = ref []
      val y_ref = ref []
      val common_ref = ref []

      fun f (ant : path) p (LAtom _) =
            ( case p of
                   [] => common_ref := ant :: !common_ref
                 | _  => y_ref := ant :: !y_ref
            )
        | f ant p (LStr r) =
            case p of
                 []      => x_ref := ant :: !x_ref
               | l :: p' =>
                   case Record.lookup l r of
                        NONE     => x_ref := (Path.append ant p) :: !x_ref
                      | SOME loc => f (Path.extend ant l) p' loc

      fun g (ant : path) p (LAtom _) = ()
        | g ant p (LStr r) =
            case p of
                 []      => y_ref := ant :: !y_ref
               | l :: p' =>
                   case Record.lookup l r of
                        NONE     => y_ref := (Path.append ant p) :: !y_ref
                      | SOME loc => g (Path.extend ant l) p' loc
    in
      app (fn p => f Path.empty (Path.to_list p) y) d1;
      app (fn p => g Path.empty (Path.to_list p) x) d2;
      { x = !x_ref
      , y = !y_ref
      , common = !common_ref
      }
    end

    exception NotStructure of modtemp

    fun lookup_modtemp l =
      fn Str r => (valOf (Record.lookup l r) handle Option => raise MissingLabel l)
       | t     => raise NotStructure t

    fun abs t = t (* TODO: Change this function when unit imports are introduced. *)

    exception CannotMerge of modtemp * modtemp

    fun x + y =
      case (x, y) of
           (Type k1, Type k2) =>
             if k1 = k2
             then Type k1
             else raise KindMismatch(k1, k2)
         | (Str r1, Str r2) =>
             let
               fun f acc l t =
                 case Record.lookup l r1 of
                      NONE => Record.insert l t acc
                    | SOME t' => Record.insert l (t' + t) acc
             in
               Str (Record.fold_left f r1 r2)
             end
         | (Str r, _) =>
             if Record.is_empty r
             then y
             else raise CannotMerge(x, y)
         | (_, Str r) =>
             if Record.is_empty r
             then x
             else raise CannotMerge(x, y)
         | _ => raise CannotMerge(x, y)
         (* TODO: Change this function when unit imports are introduced. *)

    (* Units are ignored. *)
    fun dom (Type _) : path list = [Path.empty]
      | dom (Unit _) = []
      | dom (Str r)  =
          let
            fun f acc l t =
              map (Path.prepend l) (dom t)
              @
              acc
          in
            Record.fold_left f [] r
          end

    fun remove (by : path) (ps : path list) : path list =
      foldr (fn (p, acc) => if Path.start_with by p then acc else p :: acc) [] ps

    exception CannotMergeLocator of loctemp * loctemp

    fun merge_loctemp x y : loctemp =
      case (x, y) of
           (LStr r1, LStr r2) => LStr (Record.union_with merge_loctemp r1 r2)
         | (LAtom k1, LAtom k2) =>
             if k1 = k2
             then LAtom k1
             else raise KindMismatch(k1, k2)
         | (LStr r, _) =>
             if Record.is_empty r
             then y
             else raise CannotMergeLocator(x, y)
         | (_, LStr r) =>
             if Record.is_empty r
             then x
             else raise CannotMergeLocator(x, y)
  end

  (* This indicates that there are some bugs. *)
  exception UnexpectedLocatorError

  fun erase s =
    case s of
         Type ty => Template.Type (kind_of ty)
       | Val _   => Template.Str Record.empty
       | Unit u  => Template.Unit (erase_unit u)
       | Str r   => Template.Str (Record.map erase r)

  and erase_unit (is, es, s) =
  let
    fun x + y =
    let
      fun g acc l loc =
        case Record.lookup l acc of
             NONE => Record.insert l loc acc
           | SOME loc' => Record.insert l (loc + loc') acc
      open Template
    in
      case (x, y) of
           (LStr r1, LStr r2) => LStr (Record.fold_left g r1 r2)
           (* TODO: Perhaps we should accept empty structure cases. *)
         | _ => raise UnexpectedLocatorError
    end

    fun f ((k, p : path), acc) =
      acc +
      foldr
      (fn (l, acc) => Template.LStr (Record.singleton l acc))
      (Template.LAtom k)
      (Path.to_list p)

    val l = foldl f (Template.LStr Record.empty) is

    val ifvs = map (fn (k, _) => FVar.fresh k) is
    val efvs = map FVar.fresh es
    val s' = erase (open_at_modsig 0 (map TFree efvs) (open_at_modsig 1 (map TFree ifvs) s))
  in
    (l, es, s')
  end

  fun loctemp_to_realizer (ant : path) (acc : (fvar * (kind * path)) list ref) loc : realizer =
    case loc of
         Template.LStr r  =>
           RStr (Record.map_with_key (fn l => loctemp_to_realizer (Path.extend ant l) acc) r)
       | Template.LAtom k =>
           let val fv = FVar.fresh k in
             RAtom (TFree fv) before acc := (fv, (k, ant)) :: !acc
           end

  val loctemp_to_realizer =
    loctemp_to_realizer Path.empty

  fun loctemp_to_realizer_with (ant : path) (Template.LAtom k) (is : fvar list) : realizer * fvar list * (fvar * path) list =
        (RAtom(TFree (hd is)), tl is, [(hd is, ant)])
    | loctemp_to_realizer_with ant (Template.LStr r) is =
        let
          fun f (acc, is, zs) l loc =
          let val (r, is, vps) = loctemp_to_realizer_with (Path.extend ant l) loc is in
            (Record.insert l r acc, is, zs @ vps)
          end

          val (r, is, vps) = Record.fold_left f (Record.empty, is, []) r
        in
          (RStr r, is, vps)
        end

  val loctemp_to_realizer_with = loctemp_to_realizer_with Path.empty

  exception NotStructureRealizer of realizer

  fun select (acc : (fvar * path) list ref) ant (loc : Template.loctemp) (r : realizer) =
    case loc of
         Template.LAtom k =>
           let in
             case r of
                  RAtom ty => RAtom ty
                | RStr _   =>
                    let val fv = FVar.fresh k in
                      acc := (fv, ant) :: !acc;
                      RAtom (TFree fv)
                    end
           end
       | Template.LStr r' =>
           let
             fun f l loc =
               case r of
                    RAtom _ => raise NotStructureRealizer r
                  | RStr r  =>
                      case Record.lookup l r of
                           SOME r => select acc (Path.extend ant l) loc r
                         | NONE   => select acc (Path.extend ant l) loc (RStr Record.empty)
           in
             RStr (Record.map_with_key f r')
           end

  val select : Template.loctemp -> realizer -> realizer * (fvar * path) list = fn loc => fn r =>
    let
      val acc = ref []
      val r' = select acc Path.empty loc r
    in
      (r', !acc)
    end
end
