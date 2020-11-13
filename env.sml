functor Env (
  type modsig
  type modtemp
  type ty
  val erase : modsig -> modtemp
  structure ModVar : ORDERED
  structure TVar : ORDERED

  structure Subst : sig
    type t

    val apply : t -> ty -> ty
    val apply_modsig : t -> modsig -> modsig
  end
) :> sig
  type t

  exception Unbound of ModVar.t
  exception UnboundTVar of TVar.t

  val initial : t

  val static : t -> t
  val is_static : t -> bool

  val lookup : t -> ModVar.t -> modsig
  val insert : ModVar.t -> modsig -> t -> t
  val insert_opt : ModVar.t option -> modsig -> t -> t

  val apply : Subst.t -> t -> t

  structure Type : sig
    val lookup : t -> TVar.t -> fvar
    val insert : TVar.t -> fvar -> t -> t
  end

  structure Val : sig
    type var = string
    exception Unbound of var

    val lookup : t -> var -> ty
    val insert : var -> ty -> t -> t
  end

  structure Template : sig
    type t

    val lookup : t -> ModVar.t -> modtemp
    val insert : ModVar.t -> modtemp -> t -> t
    val insert_opt : ModVar.t option -> modtemp -> t -> t

    structure Type : sig
      val lookup : t -> TVar.t -> fvar
      val insert : TVar.t -> fvar -> t -> t
    end
  end

  val erase : t -> Template.t
end = struct
  structure M = BinarySearchMap (ModVar)
  structure T = BinarySearchMap (TVar)
  structure V = BinarySearchMap (struct type t = string open String end)

  datatype mode
    = Main
    | Static

  type t =
    { m : modsig M.t
    , t : fvar T.t
    , v : ty V.t
    , mode : mode
    }

  exception Unbound of ModVar.t
  exception UnboundTVar of TVar.t

  val initial =
    { m = M.empty
    , t = T.empty
    , v = V.empty
    , mode = Main
    }

  fun static ({m, t, v, ...} : t) =
    { m = m
    , t = t
    , v = v
    , mode = Static
    }

  fun is_static (env : t) =
    Static = #mode env

  fun lookup ({m, ...} : t) v =
    case M.lookup v m of
         NONE   => raise Unbound v
       | SOME s => s

  fun insert mv s ({m, t, v, mode} : t) =
    { m = M.insert mv s m
    , t = t
    , v = v
    , mode = mode
    }

  fun insert_opt (SOME v) s env = insert v s env
    | insert_opt NONE _ env     = env

  fun apply subst ({m, t, v, mode} : t) =
    { m = M.map (Subst.apply_modsig subst) m
    , t = t
    , v = V.map (Subst.apply subst) v
    , mode = mode
    }

  structure Type = struct
    fun lookup ({t, ...} : t) v =
      valOf (T.lookup v t)
      handle Option => raise UnboundTVar v

    fun insert v fv ({m, t, v = vm, mode} : t) =
      {m = m, t = T.insert v fv t, v = vm, mode = mode}
  end

  structure Val = struct
    type var = string
    exception Unbound of var

    fun lookup ({v, ...} : t) var =
      valOf (V.lookup var v)
      handle Option => raise Unbound var

    fun insert var ty (e : t) =
      {v = V.insert var ty (#v e), m = #m e, t = #t e, mode = #mode e}
  end

  structure Template = struct
    type t =
      { m : modtemp M.t
      , t : fvar T.t
      }

    fun lookup ({m, ...} : t) v =
      case M.lookup v m of
           NONE   => raise Unbound v
         | SOME s => s

    fun insert v t ({m, t = tm} : t) =
      {m = M.insert v t m, t = tm}

    fun insert_opt (SOME v) t env = insert v t env
      | insert_opt NONE _ env     = env

    structure Type = struct
      fun lookup ({t, ...} : t) v =
        valOf (T.lookup v t)
        handle Option => raise UnboundTVar v

      fun insert v fv ({m, t} : t) =
        {m = m, t = T.insert v fv t}
    end
  end

  val erase = fn ({m, t, ...} : t) => {m = M.map erase m, t = t}
end
