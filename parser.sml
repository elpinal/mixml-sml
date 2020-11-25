structure ParserError = struct
  exception UnexpectedEOF
  exception UnexpectedToken of location * Token.t
end

structure Parser = MakeParser (
  structure Streamable = CoercedStreamable(
    structure Streamable = StreamStreamable
    type 'a item = 'a loc
    fun coerce x = unloc x
  )
  structure Arg = struct
    open Syntax
    open ParserError

    datatype terminal = datatype Token.t

    type string = string
    type int = int
    type module = module
    type modvar = ModVar.t
    type label = Label.t
    type exp = exp
    type exps = exp list
    type param = pattern * ty
    type params = param list
    type pattern = pattern
    type patterns = pattern list
    type branch = branch
    type branches = branches
    type ty = ty
    type types = ty list
    type type_var = TVar.t
    type sum = ty Sum.t
    type con = con
    type kind = kind
    type none = unit

    fun module_id m = m
    val mvar = MVar
    fun empty_module () = MEmpty
    fun exp_export e = MVE e
    val exp_import = MVI
    val type_export = MTE
    val type_import = MTI
    val unit_export = MUnit
    val minj = MInj
    val mproj = MProj
    val mnew = MNew
    fun mlink (v, x, y) = MLink(SOME v, x, y)
    fun mseal (v, x, y) = MSeal(SOME v, x, y)
    fun mlink_novar (x, y) = MLink(NONE, x, y)
    fun mseal_novar (x, y) = MSeal(NONE, x, y)

    fun mtype_binding (l, ty) = MInj(l, MTE ty)
    fun mtype_spec (l, k) = MInj(l, MTI k)
    fun mval_binding (l, e) = MInj(l, MVE e)
    fun mval_spec (l, ty) = MInj(l, MVI ty)

    fun mvar_upper s = ModVar.from_string s
    fun mvar_lower s = ModVar.from_string s

    fun label_upper s = Label.from_string s
    fun label_lower s = Label.from_string s

    fun exp_id x = x
    fun emodule m = EVal m
    fun etrue () = ELit (LBool true)
    fun efalse () = ELit (LBool false)
    fun eint n = ELit (LInt n)
    fun eunit n = ELit LUnit
    fun evar s = EVar s
    val eabs = EAbs
    val eapp = EApp
    val econ = ECon
    val ematch = EMatch

    fun etuple [x] = x
      | etuple xs = ETuple xs
    fun exps1 x = [x]
    val exps_cons = op::

    fun param1 (x, y) = (x, y)
    fun params1 x = [x]
    fun params_cons (x, xs) = x :: xs

    fun pattern_id x = x
    fun pvar v = PVar v
    fun pwildcard () = PWildcard
    val pcon = PCon
    fun punit () = PUnit

    fun ptuple [x] = x
      | ptuple xs = PTuple xs
    fun patterns1 x = [x]
    val patterns_cons = op::

    fun none_id () = ()

    val branch1 = Branch
    fun branches_nil () = []
    val branches_cons = op::

    fun type_id x = x
    fun tmodule m = TTyp m
    val tvar = TVar
    val tabs = TAbs
    val tapp = TApp
    val tarrow = TArrow
    fun tbool () = TBase Bool
    fun tint () = TBase Int
    fun tunit () = TBase Unit
    val tsum = TSum

    fun ttuple [x] = x
      | ttuple xs = TTuple xs
    fun ttuple1 x = [x]
    val ttuple_cons = op::

    val quote_tvar = TVar.from_string

    fun sum1 (c, ty) = Sum.singleton c ty
    fun sum_cons (c, ty, s) = Sum.insert c ty s

    fun con_upper s = Constructor.from_string s

    fun kbase () = KBase
    val karrow = KArrow

    fun error (s : Token.t loc Stream.stream) =
    let open Stream in
      case front s of
           Nil                => UnexpectedEOF
         | Cons(Loc(l, t), _) => UnexpectedToken(l, t)
    end
  end
)
