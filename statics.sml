structure Statics = struct
  open Std

  structure M = struct
    type label = Label.t
    type base = Syntax.base
    type con = con

    val show_label = Label.show
    val show_base = Syntax.show_base
    val show_con = Constructor.show

    structure Record = BinarySearchMap(Label)
    structure Sum = Sum
  end

  structure SemObj = SemObj(M)
  open SemObj

  structure Env = Env(
    type modsig = modsig
    type modtemp = Template.modtemp
    type ty = ty
    val erase = erase
    structure ModVar = ModVar
    structure TVar = Syntax.TVar
    structure Subst = Subst
  )

  structure VMap = BinarySearchMap (struct type t = Syntax.var open String end)

  exception NotUnit of Template.modtemp
  exception NotAtomicType of Template.modtemp
  exception NotArrowKind of kind

  fun get_template (env : Env.Template.t) : Syntax.module -> Template.unittemp =
  let open Template in
    fn Syntax.MVar v => (LStr Record.empty, [], Env.Template.lookup env v)
     | Syntax.MEmpty => (LStr Record.empty, [], Str Record.empty)
     | Syntax.MTI k  => (LAtom k, [], Type k)
     | Syntax.MTE ty => (LStr Record.empty, [], Type $ get_template_type env ty)
     | Syntax.MVI _  => (LStr Record.empty, [], Str Record.empty)
     | Syntax.MVE _  => (LStr Record.empty, [], Str Record.empty)
     | Syntax.MInj(l, m) =>
         let val (loc, ks, t) = get_template env m in
           (LStr $ Record.singleton l loc, ks, Str $ Record.singleton l t)
         end
     | Syntax.MProj(m, l) =>
         let val (loc, ks, t) = get_template env m in
           (* TODO: check that no imports are neglected? *)
           (lookup_loctemp l loc, ks, lookup_modtemp l t)
         end
     | Syntax.MLink(v, m1, m2) =>
         let
           val (loc1, ks1, t1) = get_template env m1
           val (loc2, ks2, t2) = get_template (Env.Template.insert_opt v (abs t1) env) m2
           val {x, y, common : path list} = split loc1 loc2
           val d2 = dom t2
           val d1 = dom t1
           val a = foldl (fn (by, acc) => remove by acc) x d2
           val b = foldl (fn (by, acc) => remove by acc) y d1

           val z0 = filter_loctemp common loc1
           val z1 = filter_loctemp a loc1
           val z2 = filter_loctemp b loc2
         in
           (merge_loctemp z0 (merge_loctemp z1 z2), ks1 @ ks2, t1 + t2)
         end
     | Syntax.MSeal(_, m, _) =>
         let
           val (loc, ks, t) = get_template env m
           val ks' = get_kinds loc
         in
           (LStr Record.empty, ks @ ks', abs t)
         end
     | Syntax.MUnit m =>
         let
           val ut = get_template env m
         in
           (LStr Record.empty, [], Unit ut)
         end
     | Syntax.MNew m =>
         let
           val (loc, _, t) = get_template env m
         in
           case t of
                Unit ut => ut (* TODO: check that `loc` is empty? *)
              | _       => raise NotUnit t
         end
  end

  and get_template_type env : Syntax.ty -> kind =
  let open Template in
    fn Syntax.TTyp m =>
         let
           (* TODO: check that `loc` is empty? *)
           val (loc, _, t) = get_template env m
         in
           case t of
                Type k => k
              | _      => raise NotAtomicType t
         end
     | Syntax.TVar v => FVar.get_kind $ Env.Template.Type.lookup env v
     | Syntax.TAbs(v, k, x) =>
         KArrow(
           k,
           get_template_type (Env.Template.Type.insert v (FVar.fresh k) env) x
         )
     | Syntax.TApp(x, _) =>
         let
           val k1 = get_template_type env x
         in
           case k1 of
                KArrow(_, k12) => k12
              | _              => raise NotArrowKind k1
         end
     | Syntax.TArrow _ => KBase
     | Syntax.TBase _ => KBase
     | Syntax.TSum _ => KBase
  end

  exception CannotRealize of realizer
  exception NotStructure of modsig
  exception ProjImport of modsig
  exception NotArrowType of ty
  exception NotSumType of ty
  exception NotSummand of con * ty
  exception EscapingLocalAbstractType of fvar

  structure E = struct
    exception NotAtomicType of modsig
    exception NotAtomicExp of modsig
    exception NotUnit of modsig
  end

  fun elaborate env r es =
    fn Syntax.MVar v => Env.lookup env v
     | Syntax.MEmpty => Str Record.empty
     | Syntax.MTI k  =>
         let in
           case r of
                RAtom ty => Type ty before kindcheck ty k
              | RStr _   => raise CannotRealize(r)
         end
     | Syntax.MTE ty => Type $ elaborate_type env ty
     | Syntax.MVI ty =>
         let
           val ty = elaborate_type env ty
           val () = kindcheck ty KBase
         in
           Val(ty, Import)
         end
     | Syntax.MVE e =>
         if Env.is_static env
         then Val(TBottom, Export)
         else
           let
             val ty = elaborate_exp env e
             val () = kindcheck ty KBase
           in
             Val(ty, Export)
           end
     | Syntax.MInj(l, m) =>
         let in
           case r of
                RAtom _ => raise CannotRealize(r)
              | RStr r =>
                Str $ Record.singleton l $
                elaborate env
                  let in
                  case Record.lookup l r of
                       NONE => RStr Record.empty
                     | SOME r => r
                  end
                  es
                  m
         end
     | Syntax.MProj(m, l) =>
         let
           val s = elaborate env (RStr $ Record.singleton l r) es m
         in
           case s of
                Str r =>
                  (case Record.lookup l r of
                        NONE => raise MissingLabel(l)
                      | SOME s => s before must_be_absolute (Str $ Record.delete l r)
                  )
              | _ => raise NotStructure(s)
         end
     | Syntax.MLink(v, m1, m2) =>
         let
           val (loc1, ks1, t1) = get_template (Env.erase env) m1
           val es1 = List.take (es, length ks1)
           val es2 = List.drop (es, length ks1)
           val (loc2, ks2, t2) =
             get_template (env |> Env.erase |> Env.Template.insert_opt v (Template.abs t1)) m2

           val (rx, fvs1 : (fvar * path) list) = select loc1 r
           val (ry, fvs2 : (fvar * path) list) = select loc2 r

           val s1 = elaborate env rx es1 m1
           val s1' = abs s1
           val s2' = elaborate (env |> Env.insert_opt v s1' |> Env.static) ry es2 m2
           val subst = bidirectional_lookup (fvs1, s1) (fvs2, s2')
           val s1d = Subst.apply_modsig subst s1
           val s2 = elaborate (env |> Env.insert_opt v (abs s1d)) (Subst.apply_realizer subst ry) es2 m2
         in
           merge s1d s2
         end
     | Syntax.MSeal(v, m1, m2) =>
         let
           val (loc1, ks1, t1) = get_template (Env.erase env) m1
           val es1 = List.take (es, length ks1)
           val is1 = List.drop (es, length ks1)

           val (r1, _, fvs1) = loctemp_to_realizer_with loc1 is1

           val s1 = elaborate env r1 es1 m1
         in
           if Env.is_static env
           then abs s1
           else
             let
               val (loc2, ks2, t2) =
                 get_template (env |> Env.erase |> Env.Template.insert_opt v (Template.abs t1)) m2

               val acc : (fvar * (kind * path)) list ref = ref []
               val r2 = loctemp_to_realizer acc loc2
               val fvs2 = map (fn (fv, (_, p)) => (fv, p)) $ !acc

               val s1' = abs s1
               val es2 = map FVar.fresh ks2
               val s2' = elaborate (env |> Env.insert_opt v s1' |> Env.static) r2 es2 m2
               val subst = bidirectional_lookup (fvs1, s1) (fvs2, s2')
               val s1d = Subst.apply_modsig subst s1
               val s2 = elaborate (env |> Env.apply subst |> Env.insert_opt v (abs s1d)) (Subst.apply_realizer subst r2) es2 m2
               val () = must_be_absolute $ merge s1d s2
             in
               abs s1
             end
         end
     | Syntax.MUnit m => Unit $ elaborate_unit env m
     | Syntax.MNew m =>
         let
           fun f (_, p) : ty = lookup_realizer p r
         in
           case elaborate_complete env m of
                Unit(is, ks, s) => open_at_modsig 1 (map f is) $ open_at_modsig 0 (map TFree es) s
              | s               => raise E.NotUnit s
         end

  and elaborate_type env : Syntax.ty -> ty =
    fn Syntax.TTyp m =>
         let in
           case elaborate_complete env m of
                Type ty => ty
              | s       => raise E.NotAtomicType s
         end
     | Syntax.TVar v => TFree $ Env.Type.lookup env v
     | Syntax.TAbs(v, k, x) =>
         let
           val fv = FVar.fresh k
           val ty = elaborate_type (env |> Env.Type.insert v fv) x
         in
           TAbs(k, close_at 0 [fv] ty)
         end
     | Syntax.TApp(x, y) =>
         let
           val ty1 = elaborate_type env x
           val ty2 = elaborate_type env y
         in
           case kind_of ty1 of
                KArrow(k11, k12) => TApp(ty1, ty2) before kindcheck ty2 k11
              | k1               => raise NotArrowKind k1
         end
     | Syntax.TArrow(x, y) =>
         let
           val x = elaborate_type env x
           val y = elaborate_type env y
         in
           kindcheck x KBase;
           kindcheck y KBase;
           TArrow(x, y)
         end
     | Syntax.TBase b => TBase b
     | Syntax.TSum s =>
         let fun f ty =
           let val ty = elaborate_type env ty in
            kindcheck ty KBase; ty
           end
         in
           TSum $ Sum.map f s
         end

  and elaborate_exp env : Syntax.exp -> ty =
    fn Syntax.EVal m =>
         let in
           case elaborate_complete env m of
                Val(ty, Export)     => ty
              | s as Val(_, Import) => raise ProjImport s
              | s                   => raise E.NotAtomicExp s
         end
     | Syntax.ELit l =>
         let in
           case l of
                Syntax.LBool _ => TBase Syntax.Bool
              | Syntax.LInt _  => TBase Syntax.Int
         end
     | Syntax.EVar v => Env.Val.lookup env v
     | Syntax.EAbs(ps, x) =>
         let
           val ps = map (fn (p, ty) => (p, elaborate_type env ty)) ps
           val ms = map (fn (p, ty) => left_invert env p ty) ps
           val m = foldl (fn (x, y) => VMap.union x y) VMap.empty ms
           val env' = VMap.fold_left (fn acc => fn v => fn ty => Env.Val.insert v ty acc) env m
         in
           foldr
           (fn ((_, ty), acc) => TArrow(ty, acc))
           (elaborate_exp env' x)
           ps
         end
     | Syntax.EApp(x, y) =>
         let
           val ty1 = elaborate_exp env x
           val ty2 = elaborate_exp env y
         in
           case reduce ty1 of
                TArrow(ty11, ty12) => ty12 before equal_type ty11 ty2 KBase
              | ty1'               => raise NotArrowType ty1'
         end
     | Syntax.ECon(c, x, ty) =>
         let
           val ty = elaborate_type env ty
           val ty1 = elaborate_exp env x
         in
           case reduce ty of
                TSum s =>
                  let val ty2 = (valOf (Sum.lookup c s) handle Option => raise NotSummand(c, ty)) in
                    equal_type ty1 ty2 KBase; ty
                  end
              | _ => raise NotSumType ty
         end

  and left_invert env pat ty =
    case pat of
         Syntax.PVar v => VMap.singleton v ty
       | Syntax.PWildcard => VMap.empty

  and elaborate_complete env m : modsig =
  let
    val (loc, ks, _) = get_template (Env.erase env) m
    val fvs = map FVar.fresh ks
    val s = elaborate env (RStr Record.empty) fvs m
    val vs = free_vars_modsig s
    fun f fv = Option.isSome $ FVar.Map.lookup fv vs
  in
    must_be_absolute s;
    case List.find f fvs of
         NONE    => s
       | SOME fv => raise EscapingLocalAbstractType fv
  end

  and elaborate_unit env m : unitsig =
  let
    val (loc, ks, _) = get_template (Env.erase env) m

    val acc : (fvar * (kind * path)) list ref = ref []
    val r = loctemp_to_realizer acc loc
    val fvs = map #1 $ !acc
    val is = map #2 $ !acc

    val es = map FVar.fresh ks
    val s = elaborate env r es m
  in
    (is, ks, close_at_modsig 1 fvs $ close_at_modsig 0 es s)
  end

  (* No imports, but allows abstract type exports. *)
  fun elaborate_program env m : modsig =
  let
    val (loc, ks, _) = get_template (Env.erase env) m
    val fvs = map FVar.fresh ks
    val s = elaborate env (RStr Record.empty) fvs m
  in
    must_be_absolute s;
    s
  end
end
