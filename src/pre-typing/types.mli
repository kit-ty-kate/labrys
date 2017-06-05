(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type name = Ident.Type.t
type tyvar_name = Ident.TypeVar.t

type t = PrivateTypes.t

type visibility = PrivateTypes.visibility =
  | Abstract of Kinds.t
  | Alias of (t * Kinds.t)

val of_parse_tree_kind :
  pure_arrow:[< `Allow | `Partial | `Forbid] ->
  <lib_dir : string; ..> ->
  Gamma.t ->
  DesugaredTree.ty ->
  (t * Kinds.t)

val of_parse_tree :
  pure_arrow:[< `Allow | `Partial | `Forbid] ->
  <lib_dir : string; ..> ->
  Gamma.t ->
  DesugaredTree.ty ->
  t

val to_string : t -> string

val is_subset_of : t -> t -> bool

val replace : from:tyvar_name -> ty:t -> t -> t

val is_value : t -> bool

val size : t -> int

val head : t -> (name * t list)

module TyErr : sig
  val fail : loc_t:Location.t -> has:t -> expected:t -> 'a
end

val apply :
  loc_f:Location.t ->
  loc_x:Location.t ->
  t ->
  t ->
  (Effects.t * t * t)

val apply_ty :
  loc_f:Location.t ->
  loc_x:Location.t ->
  ty_x:t ->
  kind_x:Kinds.t ->
  t ->
  t

val apply_tyclass :
  loc_x:Location.t ->
  t ->
  Ident.TyClass.t ->
  PrivateTypes.t list ->
  (t * Effects.t)

val has_io : <lib_dir : string; ..> -> t -> bool

val is_fun : t -> bool

val is_unit : <lib_dir : string; ..> -> t -> bool

val tyclass_wrap : Ident.TyClass.t -> (tyvar_name * Kinds.t) list -> t -> t

val extract_filled_tyclasses :
  t ->
  ((Ident.TyClass.t * t list) option list * Effects.t * t)

val forall : tyvar_name * Kinds.t * t -> t
val tyclass :
  (Ident.TyClass.t * Kinds.t GammaMap.TypeVar.t * PrivateTypes.t list) * PrivateTypes.effects * t ->
  t
val ty : loc:Location.t -> Gamma.t -> Ident.Type.t -> t
