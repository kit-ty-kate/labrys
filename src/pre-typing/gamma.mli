(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type t = private
  { values : PrivateTypes.t GammaMap.Value.t
  ; variants : (int * PrivateTypes.t * int) GammaMap.Variant.t
  ; types : PrivateTypes.visibility GammaMap.Types.t
  ; type_vars : Kinds.t GammaMap.TypeVar.t
  ; constructors : (Ident.TypeVar.t list * (PrivateTypes.t list * int) GammaMap.Index.t) GammaMap.Constr.t
  ; exceptions : PrivateTypes.t list GammaMap.Exn.t
  ; tyclasses : PrivateTypes.class_t GammaMap.TyClass.t
  ; named_instances : (Ident.TyClass.t * PrivateTypes.t list) GammaMap.Instance.t
  }

val empty : t

val add_value : Ident.Name.t -> PrivateTypes.t -> t -> t
val add_variant : Ident.Variant.t -> (int * PrivateTypes.t * int) -> t -> t
val add_type : Ident.Type.t -> PrivateTypes.visibility -> t -> t
val add_type_var : Ident.TypeVar.t -> Kinds.t -> t -> t
val add_constr : Ident.Type.t -> Ident.Variant.t -> Ident.TypeVar.t list -> (PrivateTypes.t list * int) -> t -> t
val add_exception : Ident.Exn.t -> PrivateTypes.t list -> t -> t
val add_tyclass : Ident.TyClass.t -> PrivateTypes.class_t -> t -> t
val add_named_instance : Ident.Instance.t -> (Ident.TyClass.t * PrivateTypes.t list) -> t -> t

val replace_tyclass : Ident.TyClass.t -> PrivateTypes.class_t -> t -> t

val union : t -> t -> t

val is_subset_of : t -> t -> string list

val get_untyped_values : t -> LIdent.t GammaMap.Value.t
