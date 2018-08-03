(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

open TypedEnv

let trigger_pure_arrow = function
  | `Partial -> `Forbid
  | `Allow | `Forbid as x -> x

let add_abstract_type name k env =
  let types = EnvMap.Type.add name (Abstract k) env.TypedEnv.types in
  {env with TypedEnv.types}

let rec kind_equal x y = match x, y with
  | KStar, KStar | KEff, KEff | KExn, KExn -> true
  | KFun (k1, k2), KFun (k1', k2') -> kind_equal k1 k1' && kind_equal k2 k2'
  | KStar, _ | KEff, _ | KExn, _ | KFun _, _ -> false

let kind_fail ~loc ~has ~expected =
  Err.fail_doc
    ~loc
    Utils.PPrint.(str "Type has kind" ^^^
                  squotes (ParseTreePrinter.dump_kind has) ^^^
                  str "but was expected of kind" ^^^
                  squotes (ParseTreePrinter.dump_kind expected) ^^^
                  dot)

let rec eff_check ~pure_arrow env (_, e) =
  List.map (check_eff ~pure_arrow env) e

and eff_arrow_check ~loc ~pure_arrow env e = match e, pure_arrow with
  | None, (`Allow | `Partial) ->
      []
  | None, `Forbid ->
      (* TODO: Warning ? *)
      Err.fail ~loc "Pure arrows are forbidden here. If you really want one \
                     use the explicit syntax '-[]->' instead"
  | Some e, (`Allow | `Partial | `Forbid) ->
      eff_check ~pure_arrow env e

and check ~pure_arrow env = function
  | (loc, PretypedTree.Fun (t1, e, t2)) ->
      let t1 = check_value ~pure_arrow:(trigger_pure_arrow pure_arrow) env t1 in
      let e = eff_arrow_check ~loc ~pure_arrow env e in
      let t2 = check_value ~pure_arrow env t2 in
      (Fun (t1, e, t2), KStar)
  | (_, PretypedTree.Ty name) ->
      begin match EnvMap.Type.find name env.TypedEnv.types with
      | Alias (k, ty) -> (ty, k)
      | Abstract k | Datatype (k, _) -> (Ty name, k)
      end
  | (_, PretypedTree.Eff e) ->
      (Eff (eff_check ~pure_arrow env e), KEff)
  | (_, PretypedTree.Sum sum) ->
      (Sum (List.map (check_exn ~pure_arrow env) sum), KStar)
  | (_, PretypedTree.Forall ((name, k), t)) ->
      let env = add_abstract_type name k env in
      (Forall (name, k, check_value ~pure_arrow env t), KStar)
  | (_, PretypedTree.TyClass _) ->
      assert false (* TODO *)
  | (_, PretypedTree.AbsOnTy ((name, k), t)) ->
      let env = add_abstract_type name k env in
      let (t, k') = check ~pure_arrow env t in
      (Abs (name, k, t), KFun (k, k'))
  | (loc, PretypedTree.AppOnTy (t1, t2)) ->
      let (ty2, k2) = check ~pure_arrow env t2 in
      begin match check ~pure_arrow env t1 with
      | (ty1, KFun (k1, k1')) when kind_equal k1 k2 -> (App (ty1, ty2), k1')
      | (_, KFun (k1, _)) -> kind_fail ~loc:(fst t2) ~has:k2 ~expected:k1
      | (_, KEff) -> Err.fail ~loc "Cannot apply on a type of kind '!'."
      | (_, KStar) -> Err.fail ~loc "Cannot apply on a type of kind '*'."
      | (_, KExn) -> Err.fail ~loc "Cannot apply on a type of kind '^'."
      end

and check_value ~pure_arrow env x =
  match check ~pure_arrow env x with
  | (ty, KStar) -> ty
  | (_, (KEff | KExn | KFun _ as k)) ->
      kind_fail ~loc:(fst x) ~has:k ~expected:KStar

and check_eff ~pure_arrow env x =
  match check ~pure_arrow env x with
  | (ty, KEff) -> ty
  | (_, (KStar | KExn | KFun _ as k)) ->
      kind_fail ~loc:(fst x) ~has:k ~expected:KEff

and check_exn ~pure_arrow env x =
  match check ~pure_arrow env x with
  | (ty, KExn) -> ty
  | (_, (KStar | KEff | KFun _ as k)) ->
      kind_fail ~loc:(fst x) ~has:k ~expected:KExn

let rec replace a ~by =
  let replace t = replace a ~by t in
  function
  | Ty b when Ident.Type.equal a b -> by
  | Ty _ as t -> t
  | Eff e -> Eff (List.map replace e)
  | Sum sum -> Sum (List.map replace sum)
  | Fun (t1, e, t2) -> Fun (replace t1, List.map replace e, replace t2)
  | Forall (b, _, _) | Abs (b, _, _) as t when Ident.Type.equal a b -> t
  | Forall (b, k, t) -> Forall (b, k, replace t)
  | Abs (b, k, t) -> Abs (b, k, replace t)
  | App (t1, t2) -> App (replace t1, replace t2)

let fresh n =
  Ident.Type.local_create ~loc:Builtins.unknown_loc (string_of_int n)

let rec list_equal n e1 e2 =
  try List.for_all2 (equal n) e1 e2 with
  | Invalid_argument _ -> false

and equal n x y = match x, y with
  | Ty a1, Ty a2 ->
      Ident.Type.equal a1 a2
  | Eff e1, Eff e2 ->
      list_equal n e1 e2
  | Sum sum1, Sum sum2 ->
      list_equal n sum1 sum2
  | Fun (t1, e1, t2), Fun (t1', e2, t2') ->
      equal n t1' t1 && list_equal n e1 e2 && equal n t2 t2'
  | Forall (a1, k1, t1), Forall (a2, k2, t2)
  | Abs (a1, k1, t1), Abs (a2, k2, t2) ->
      let by = Ty (fresh n) in
      let t1 = replace a1 ~by t1 in
      let t2 = replace a2 ~by t2 in
      kind_equal k1 k2 && equal (succ n) t1 t2
  | App (t1, t2), App (t1', t2') ->
      equal n t2 t2' && equal n t1 t1'
  | App (t1, t2), t' ->
      equal n (app t1 t2) t'
  | t, App (t1, t2) ->
      equal n t (app t1 t2)
  | Ty _, _ | Eff _, _ | Sum _, _ | Fun _, _ | Forall _, _ | Abs _, _ ->
      false

and app x y = match x, y with
  | Abs (a, _, t1), t2 ->
      replace a ~by:t2 t1
  | App (t1, t2), t' ->
      begin match app t1 t2 with
      | Abs (a, _, t) -> replace a ~by:t' t
      | App _ | Ty _ | Eff _ | Sum _ | Fun _ | Forall _ as t -> App (t, t')
      end
  | Ty _ | Eff _ | Sum _ | Fun _ | Forall _ as t1, t2 ->
      App (t1, t2)

let equal = equal 0

let rec head = function
  | Ty name -> Some name
  (* TODO: Sum ? *)
  | Eff _ | Sum _ | Fun _ | Forall _ | Abs _ -> None
  | App (x, _) -> head x

let loc = Builtins.unknown_loc

let rec to_ptype = function
  | Fun (t1, e, t2) ->
      (loc, PretypedTree.Fun (to_ptype t1, Some (to_ptype_eff e), to_ptype t2))
  | Ty name -> (loc, PretypedTree.Ty name)
  | Eff e -> (loc, PretypedTree.Eff (to_ptype_eff e))
  | Sum sum -> (loc, PretypedTree.Sum (List.map to_ptype sum))
  | Forall (name, k, t) -> (loc, PretypedTree.Forall ((name, k), to_ptype t))
  | Abs (name, k, t) -> (loc, PretypedTree.AbsOnTy ((name, k), to_ptype t))
  | App (t1, t2) -> (loc, PretypedTree.AppOnTy (to_ptype t1, to_ptype t2))

and to_ptype_eff e = (loc, List.map to_ptype e)

let dump ty = PretypedTreePrinter.dump_ty (to_ptype ty)
let dump_eff eff =
  PretypedTreePrinter.dump_ty (loc, PretypedTree.Eff (to_ptype_eff eff))
