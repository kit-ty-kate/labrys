(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type index = int

type kind = PretypedTree.kind =
  | KStar
  | KEff
  | KFun of (kind * kind)

type effects = ty list

and ty =
  | TAlias of (Ident.Type.t * ty)
  | Ty of Ident.Type.t
  | Eff of effects
  | Fun of (ty * effects * ty)
  | Forall of (Ident.Type.t * kind * ty)
  | Abs of (Ident.Type.t * kind * ty)
  | App of (ty * ty)

type aty =
  | Abstract of PretypedTree.kind
  | Alias of ty
  | Datatype of (PretypedTree.kind * (Ident.Constr.t * index * ty) list)

type env = {
  values : ty EnvMap.Value.t;
  constrs : (index * ty) EnvMap.Constr.t;
  types : aty EnvMap.Type.t;
  exns : ty list EnvMap.Exn.t;
}
