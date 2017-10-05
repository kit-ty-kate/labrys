(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type index = int

type kind = PretypedTree.kind =
  | KStar
  | KEff
  | KExn
  | KFun of (kind * kind)

type effects = ty list

(* Checked types *)
and ty =
  | Ty of Ident.Type.t
  | Eff of effects
  | Sum of ty list
  | Fun of (ty * effects * ty)
  | Forall of (Ident.Type.t * kind * ty)
  | Abs of (Ident.Type.t * kind * ty)
  | App of (ty * ty)

type neffects = nty list

(* Checked and normalized types *)
and nty =
  | NTy of Ident.Type.t
  | NSum of nty list
  | NFun of (nty * neffects * nty)
  | NForall of (Ident.Type.t * kind * nty)
  | NApp of (nty * ty)

type aty =
  | Abstract of PretypedTree.kind
  | Alias of (PretypedTree.kind * ty)
  | Datatype of (PretypedTree.kind * (Ident.Constr.t * index * nty) list)

type constr_rep = Index of index | Exn

type env = {
  values : nty EnvMap.Value.t;
  constrs : (constr_rep * nty) EnvMap.Constr.t;
  types : aty EnvMap.Type.t;
}
