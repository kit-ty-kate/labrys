(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type name = Ident.Name.t
type t_name = Ident.Type.t
type tyvar_name = Ident.TypeVar.t
type exn_name = Ident.Exn.t
type tyclass_name = Ident.TyClass.t
type instance_name = Ident.Instance.t
type ty = DesugaredTree.ty
type loc = Location.t
type variant = DesugaredTree.variant
type tyclass_instance = DesugaredTree.tyclass_instance

type t =
  | Val of (name * ty)
  | AbstractType of (t_name * Kinds.t)
  | Datatype of (t_name * Kinds.t * (tyvar_name * Kinds.t) list * variant list)
  | TypeAlias of (t_name * ty)
  | Exception of (exn_name * ty list)
  | Class of (tyclass_name * (tyvar_name * Kinds.t) list * (name * ty) list)
  | Instance of (tyclass_instance * instance_name option)
