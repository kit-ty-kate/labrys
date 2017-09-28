(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type index = int

type ty = unit (* TODO *)

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
