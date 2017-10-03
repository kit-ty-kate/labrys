(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open TypedEnv

let rec normalize = function
  | Ty name -> NTy name
  | Eff _ | Abs _ -> assert false
  | Fun (t1, e, t2) -> NFun (normalize t1, List.map normalize e, normalize t2)
  | Forall (name, k, t) -> NForall (name, k, normalize t)
  | App (t1, t2) ->
      begin match Type.app t1 t2 with
      | App (t1, t2) -> NApp (normalize t1, t2)
      | Eff _ | Abs _ -> assert false
      | Ty _ | Fun _ | Forall _ as t -> normalize t
      end

let check ~pure_arrow env ty =
  let ty = Type.check_value ~pure_arrow env ty in
  normalize ty

let rec to_type = function
  | NFun (t1, e, t2) -> Fun (to_type t1, List.map to_type e, to_type t2)
  | NTy name -> Ty name
  | NForall (name, k, t) -> Forall (name, k, to_type t)
  | NApp (t1, t2) -> App (to_type t1, t2)

let rec replace a ~by ~by' =
  let replace t = replace a ~by ~by' t in
  function
  | NTy b when Ident.Type.equal a b -> by
  | NTy _ as t -> t
  | NFun (t1, e, t2) -> NFun (replace t1, List.map replace e, replace t2)
  | NForall (b, k, t) -> NForall (b, k, replace t)
  | NApp (t1, t2) -> NApp (replace t1, Type.replace a ~by:by' t2)

let replace a ~by = replace a ~by ~by':(to_type by)

let fresh n =
  Ident.Type.local_create ~loc:Builtins.unknown_loc (string_of_int n)

let rec eff_is_subset_of n e1 e2 = match e1, e2 with
  | [], _ -> true
  | t::e1, e2 -> List.exists (is_subset_of n t) e2 && eff_is_subset_of n e1 e2

and is_subset_of n x y = match x, y with
  | NTy a1, NTy a2 ->
      Ident.Type.equal a1 a2
  | NFun (t1, e1, t2), NFun (t1', e2, t2') ->
      is_subset_of n t1' t1 && eff_is_subset_of n e1 e2 && is_subset_of n t2 t2'
  | NForall (a1, k1, t1), NForall (a2, k2, t2) ->
      let by = NTy (fresh n) in
      let t1 = replace a1 ~by t1 in
      let t2 = replace a2 ~by t2 in
      Type.kind_equal k1 k2 && is_subset_of (succ n) t1 t2
  | NApp (t1, t2), NApp (t1', t2') ->
      Type.equal t2 t2' && is_subset_of n t1 t1'
  | NTy _, _ | NFun _, _ | NForall _, _ | NApp _, _ ->
      false

let is_subset_of = is_subset_of 0

let datatype_is_subset_of (k1, constrs1) (k2, constrs2) =
  let aux (name1, _, ty1) (name2, _, ty2) =
    Ident.Constr.equal name1 name2 && is_subset_of ty1 ty2
  in
  Type.kind_equal k1 k2 && List.equal aux constrs1 constrs2

let aty_is_subset_of x y = match x, y with
  | Abstract k1, Abstract k2
  | Abstract k1, Alias (k2, _)
  | Abstract k1, Datatype (k2, _) -> Type.kind_equal k1 k2
  | Alias (_, t1), Alias (_, t2) -> Type.equal t1 t2
  | Datatype d1, Datatype d2 -> datatype_is_subset_of d1 d2
  | Alias _, _ | Datatype _, _ -> false

let is_subset_of_list tys1 tys2 =
  try List.for_all2 is_subset_of tys1 tys2 with
  | Invalid_argument _ -> false

open Utils.PPrint

let dump_ty_name name = str (Ident.Type.to_string name)
let dump_exn_name name = str (Ident.Exn.to_string name)
let dump_constr_name name = str (Ident.Constr.to_string name)
let dump_kind = ParseTreePrinter.dump_kind

let dump ty = Type.dump (to_type ty)

let dump_constr (name, _, ty) =
  bar ^^^ dump_constr_name name ^^^ colon ^^^ dump ty

let dump_constrs constrs =
  separate_map hardline dump_constr constrs

let dump_aty name aty =
  str "type" ^^^ dump_ty_name name ^^^
  match aty with
  | Abstract k ->
      colon ^^^ dump_kind k
  | Alias (_, ty) ->
      equals ^//^ Type.dump ty
  | Datatype (k, constrs) ->
      colon ^^^ dump_kind k ^^^ equals ^//^ dump_constrs constrs

let dump_exn name tys =
  str "exception" ^^^ dump_exn_name name ^^^ separate_map space dump tys
