(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type t = private
  { values : PrivateTypes.t EnvMap.Value.t
  ; variants : (int * PrivateTypes.t * int) EnvMap.Variant.t
  ; types : PrivateTypes.visibility EnvMap.Types.t
  ; type_vars : PrivateTypes.kind EnvMap.TypeVar.t
  ; constructors : (Ident.TypeVar.t list * (PrivateTypes.t list * int) EnvMap.Index.t) EnvMap.Constr.t
  ; exceptions : PrivateTypes.t list EnvMap.Exn.t
  ; tyclasses : PrivateTypes.class_t EnvMap.TyClass.t
  ; named_instances : (Ident.TyClass.t * PrivateTypes.t list) EnvMap.Instance.t
  }

val empty : t

val add_value : Ident.Name.t -> PrivateTypes.t -> t -> t
val add_variant : Ident.Variant.t -> (int * PrivateTypes.t * int) -> t -> t
val add_type : Ident.Type.t -> PrivateTypes.visibility -> t -> t
val add_type_var : Ident.TypeVar.t -> PrivateTypes.kind -> t -> t
val add_constr : Ident.Type.t -> Ident.Variant.t -> Ident.TypeVar.t list -> (PrivateTypes.t list * int) -> t -> t
val add_exception : Ident.Exn.t -> PrivateTypes.t list -> t -> t
val add_tyclass : Ident.TyClass.t -> PrivateTypes.class_t -> t -> t
val add_named_instance : Ident.Instance.t -> (Ident.TyClass.t * PrivateTypes.t list) -> t -> t

val replace_tyclass : Ident.TyClass.t -> PrivateTypes.class_t -> t -> t

val union : t -> t -> t

val is_subset_of : t -> t -> string list

val get_untyped_values : t -> LIdent.t EnvMap.Value.t
