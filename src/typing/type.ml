(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open TypedEnv

let check ~pure_arrow env x =
  assert false (* TODO *)

let check_value ~pure_arrow env x =
  match check ~pure_arrow env x with
  | (ty, KStar) ->
      ty
  | (_, (KEff | KFun _)) ->
      Err.fail
        ~loc:(fst x)
        "Values cannot have types with a kind different from '*'"

let rec kind_equal x y = match x, y with
  | KStar, KStar | KEff, KEff -> true
  | KFun (k1, k2), KFun (k1', k2') -> kind_equal k1 k1' && kind_equal k2 k2'
  | KStar, _ | KEff, _ | KFun _, _ -> false

let rec replace a ~by =
  let replace t = replace a ~by t in
  function
  | Ty b when Ident.Type.equal a b -> by
  | TAlias _ | Ty _ as t -> t
  | Eff e -> Eff (List.map replace e)
  | Fun (t1, e, t2) -> Fun (replace t1, List.map replace e, replace t2)
  | Forall (b, _, _) | Abs (b, _, _) as t when Ident.Type.equal a b -> t
  | Forall (b, k, t) -> Forall (b, k, replace t)
  | Abs (b, k, t) -> Abs (b, k, replace t)
  | App (t1, t2) -> App (replace t1, replace t2)

let fresh n =
  Ident.Type.local_create ~loc:Builtins.unknown_loc (string_of_int n)

let rec eff_is_subset_of n e1 e2 = match e1, e2 with
  | [], _ -> true
  | t::e1, e2 -> List.exists (is_subset_of n t) e2 && eff_is_subset_of n e1 e2

and is_subset_of n x y = match x, y with
  | TAlias (_, x), y | x, TAlias (_, y) ->
      is_subset_of n x y
  | Ty a1, Ty a2 ->
      Ident.Type.equal a1 a2
  | Eff e1, Eff e2 ->
      eff_is_subset_of n e1 e2
  | Fun (t1, e1, t2), Fun (t1', e2, t2') ->
      is_subset_of n t1' t1 && eff_is_subset_of n e1 e2 && is_subset_of n t2 t2'
  | Forall (a1, k1, t1), Forall (a2, k2, t2)
  | Abs (a1, k1, t1), Abs (a2, k2, t2) ->
      let by = Ty (fresh n) in
      let t1 = replace a1 ~by t1 in
      let t2 = replace a2 ~by t2 in
      kind_equal k1 k2 && is_subset_of (succ n) t1 t2
  | App (t1, t2), App (t1', t2') ->
      is_subset_of n t2 t2' && is_subset_of n t2' t2 && is_subset_of n t1 t1'
  | App (t1, t2), t' ->
      is_subset_of n (app t1 t2) t'
  | t, App (t1, t2) ->
      is_subset_of n t (app t1 t2)
  | Ty _, _ | Eff _, _ | Fun _, _ | Forall _, _ | Abs _, _ ->
      false

and app x y = match x, y with
  | Abs (a, _, t1), t2 ->
      replace a ~by:t2 t1
  | App (t1, t2), t' ->
      begin match app t1 t2 with
      | Abs (a, _, t) -> replace a ~by:t' t
      | TAlias (_, t) -> app t t'
      | App _ | Ty _ | Eff _ | Fun _ | Forall _ as t -> App (t, t')
      end
  | TAlias (_, t1), t2 ->
      app t1 t2
  | Ty _ | Eff _ | Fun _ | Forall _ as t1, t2 ->
      App (t1, t2)

let is_subset_of = is_subset_of 0
