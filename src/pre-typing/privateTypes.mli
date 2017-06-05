(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

module Exn_set : Utils.EQSET with type elt = Ident.Exn.t
module Variables : Utils.EQSET with type elt = Ident.TypeVar.t
module Effects : Utils.EQSET with type elt = Ident.Type.t

type name = Ident.Name.t
type ty_name = Ident.Type.t
type tyvar_name = Ident.TypeVar.t

type effects =
  { variables : Variables.t
  ; effects : Effects.t
  ; exns : Exn_set.t
  }

type t =
  | Ty of ty_name
  | TyVar of tyvar_name
  | Eff of effects
  | Fun of (t * effects * t)
  | Forall of (tyvar_name * Kinds.t * t)
  | TyClass of ((Ident.TyClass.t * Kinds.t GammaMap.TypeVar.t * t list) * effects * t)
  | AbsOnTy of (tyvar_name * Kinds.t * t)
  | AppOnTy of (t * t)

type visibility =
  | Abstract of Kinds.t
  | Alias of (t * Kinds.t)

val ty_equal : t -> t -> bool

val ty_is_subset_of : t -> t -> bool

val ty_to_string : t -> string

val tyclass_args_equal :
  (tyvar_name list * t) list ->
  (tyvar_name list * t) list ->
  bool

val ty_reduce : t -> t

val ty_replace : from:Ident.TypeVar.t -> ty:t -> t -> t

val eff_union : effects -> effects -> effects

val eff_union_ty' : effects -> t -> effects
val eff_union_ty : from:Ident.TypeVar.t -> effects -> t -> effects

val eff_replace : from:Ident.TypeVar.t -> ty:t -> effects -> effects

val eff_is_subset_of : (tyvar_name * tyvar_name) list -> effects -> effects -> bool

val eff_is_empty : effects -> bool

val eff_to_string : effects -> string

module Instances : Utils.EQMAP with type key = t list

(* TODO: Handle contraints *)
type class_t =
  { params : (tyvar_name * Kinds.t) list
  ; signature : (name * t) list
  ; instances : name Instances.t
  }

val class_equal : class_t -> class_t -> bool
