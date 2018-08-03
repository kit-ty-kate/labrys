(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

open TypedEnv

let rec normalize = function
  | Ty name -> NTy name
  | Eff _ | Abs _ -> assert false
  | Sum sum -> NSum (List.map normalize sum)
  | Fun (t1, e, t2) -> NFun (normalize t1, List.concat (List.map normalize_eff e), normalize t2)
  | Forall (name, k, t) -> NForall (name, k, normalize t)
  | App (t1, t2) ->
      begin match Type.app t1 t2 with
      | App (t1, t2) -> NApp (normalize t1, t2)
      | Eff _ | Abs _ -> assert false
      | Sum sum -> NSum (List.map normalize sum)
      | Ty _ | Fun _ | Forall _ as t -> normalize t
      end

and normalize_eff = function
  | Ty name -> [NTy name]
  | Eff e -> List.concat (List.map normalize_eff e)
  | Sum _ | Fun _ | Abs _ | Forall _ -> assert false
  | App (t1, t2) ->
      begin match Type.app t1 t2 with
      | Ty name -> [NTy name]
      | Eff e -> List.concat (List.map normalize_eff e)
      | App (t1, t2) -> [NApp (normalize t1, t2)]
      | Sum _ | Fun _ | Abs _ | Forall _ -> assert false
      end

let check ~pure_arrow env ty =
  let ty = Type.check_value ~pure_arrow env ty in
  normalize ty

let check_eff ~pure_arrow env ty =
  let ty = Type.check_eff ~pure_arrow env ty in
  normalize_eff ty

let rec to_type = function
  | NFun (t1, e, t2) -> Fun (to_type t1, List.map to_type e, to_type t2)
  | NTy name -> Ty name
  | NSum sum -> Sum (List.map to_type sum)
  | NForall (name, k, t) -> Forall (name, k, to_type t)
  | NApp (t1, t2) -> App (to_type t1, t2)

let rec replace a ~by ~by' =
  let replace t = replace a ~by ~by' t in
  function
  | NTy b when Ident.Type.equal a b -> by
  | NTy _ as t -> t
  | NSum sum -> NSum (List.map replace sum)
  | NFun (t1, e, t2) -> NFun (replace t1, List.map replace e, replace t2)
  | NForall (b, k, t) -> NForall (b, k, replace t)
  | NApp (t1, t2) -> NApp (replace t1, Type.replace a ~by:by' t2)

let replace a ~by = replace a ~by ~by':(to_type by)

let fresh n =
  Ident.Type.local_create ~loc:Builtins.unknown_loc (string_of_int n)

let rec sub_is_subset_of n e1 e2 = match e1, e2 with
  | [], _ -> true
  | t::e1, e2 -> List.exists (is_subset_of n t) e2 && sub_is_subset_of n e1 e2

and is_subset_of n x y = match x, y with
  | NTy a1, NTy a2 ->
      Ident.Type.equal a1 a2
  | NSum sum1, NSum sum2 ->
      sub_is_subset_of n sum1 sum2
  | NFun (t1, e1, t2), NFun (t1', e2, t2') ->
      is_subset_of n t1' t1 && sub_is_subset_of n e1 e2 && is_subset_of n t2 t2'
  | NForall (a1, k1, t1), NForall (a2, k2, t2) ->
      let by = NTy (fresh n) in
      let t1 = replace a1 ~by t1 in
      let t2 = replace a2 ~by t2 in
      Type.kind_equal k1 k2 && is_subset_of (succ n) t1 t2
  | NApp (t1, t2), NApp (t1', t2') ->
      Type.equal t2 t2' && is_subset_of n t1 t1'
  | NTy _, _ | NSum _, _ | NFun _, _ | NForall _, _ | NApp _, _ ->
      false

let is_subset_of = is_subset_of 0
let eff_is_subset_of = sub_is_subset_of 0

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

let rec size = function
  | NTy _ | NSum _ | NApp _ -> 0
  | NFun (_, _, t) -> succ (size t)
  | NForall (_, _, t) -> size t

let replace a ~by t =
  normalize (Type.replace a ~by (to_type t))

let multi_replace assoc_tbl t =
  let aux t (a, by) = replace a ~by t in
  List.fold_left aux t assoc_tbl

let rec match_base_ty ~base_ty t = match base_ty, t with
  | a, NTy b ->
      [(b, to_type a)]
  | NApp (nty1, ty1), NApp (nty2, ty2) ->
      match_base_ty ~base_ty:nty1 nty2 @
      begin match ty1, ty2 with
      | a, Ty b -> [(b, a)]
      | _, (Eff _ | Sum _ | Fun _ | Forall _ | Abs _ | App _) -> assert false
      end
  | NTy _, _ | NSum _, _ | NApp _, _ | NForall _, _ | NFun _, _ ->
      assert false

let rec match_ty ~base_ty = function
  | NTy _ | NSum _ | NApp _ as t ->
      (match_base_ty ~base_ty t, [], base_ty)
  | NForall (a, _, t) ->
      let (assoc_tbl, l, t) = match_ty ~base_ty t in
      let assoc_tbl = List.Assoc.remove ~eq:Ident.Type.equal a assoc_tbl in
      (assoc_tbl, l, t)
  | NFun (t1, _, t2) ->
      let (assoc_tbl, l, t) = match_ty ~base_ty t2 in
      let t1 = multi_replace assoc_tbl t1 in
      (assoc_tbl, t1::l, t)

let match_ty ~base_ty t =
  let (_, args, t) = match_ty ~base_ty t in
  (args, t)

let rec monomorphic_split = function
  | NTy _ | NSum _ | NApp _ as t ->
      ([], t)
  | NForall (a, _, _) ->
      Err.fail ~loc:(Ident.Type.loc a) "Type abstractions are forbidden here"
  | NFun (t1, e, t2) ->
      let (l, t) = monomorphic_split t2 in
      ((t1, e)::l, t)

open Utils.PPrint

let dump_ty_name name = str (Ident.Type.to_string name)
let dump_constr_name name = str (Ident.Constr.to_string name)
let dump_kind = ParseTreePrinter.dump_kind

let dump ty = Type.dump (to_type ty)
let dump_eff eff = Type.dump_eff (List.map to_type eff)

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

let dump_constr name (rep, ty) =
  let name_ty = dump_constr_name name ^^^ colon ^^^ dump ty in
  match rep with
  | Index idx -> str "constructor" ^^^ name_ty ^^^ equals ^^^ OCaml.int idx
  | Exn -> str "exception" ^^^ name_ty
