structure Syntax = struct
  datatype kind = datatype kind

  structure TVar :> sig
    include ORDERED

    val from_string : string -> t
  end = struct
    type t = string

    val compare = String.compare

    fun from_string x = x
  end

  type tvar = TVar.t

  type modvar = ModVar.t

  type label = Label.t

  (* `base` always has the base kind and is free from type variables. *)
  datatype base
    = Bool
    | Int

  fun show_base Bool = "bool"
    | show_base Int  = "int"

  datatype lit
    = LBool of bool
    | LInt  of int

  type var = string

  datatype module
    = MVar of modvar
    | MEmpty
    | MVI of ty
    | MVE of exp
    | MTI of kind
    | MTE of ty
    | MInj of label * module
    | MProj of module * label
    | MUnit of module
    | MNew of module
    | MLink of modvar option * module * module
    | MSeal of modvar option * module * module

  and exp
    = EVal of module
    | ELit of lit
    | EVar of var
    | EAbs of (pattern * ty) list * exp
    | EApp of exp * exp
    | ECon of con * exp * ty

  and pattern
    = PVar of var
    | PWildcard

  and ty
    = TTyp of module
    | TVar of tvar
    | TAbs of tvar * kind * ty
    | TApp of ty * ty
    | TArrow of ty * ty
    | TBase of base
    | TSum of ty Sum.t
end
