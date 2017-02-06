(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type tyvar_name = Ident.TypeVar.t

type t = PrivateTypes.class_t

val create :
  (Ident.TypeVar.t * Kinds.t) list ->
  (Ident.Name.t * PrivateTypes.t) list ->
  t

val equal : t -> t -> bool

val get_params :
  loc:Location.t ->
  (Gamma.t -> DesugaredTree.ty -> PrivateTypes.t * Kinds.t) ->
  Gamma.t ->
  Kinds.t GammaMap.TypeVar.t ->
  DesugaredTree.ty list ->
  t ->
  (Gamma.t * PrivateTypes.t list)

val get_instance_name :
  loc:Location.t ->
  tyclass:Ident.TyClass.t ->
  PrivateTypes.t list ->
  t ->
  Ident.Name.t

val add_instance :
  tyclass:Ident.TyClass.t ->
  current_module:Module.t ->
  (PrivateTypes.t * Kinds.t) list ->
  t ->
  (Ident.Name.t * PrivateTypes.t list * t)

val get_values :
  loc:Location.t ->
  PrivateTypes.t list ->
  ((Ident.Name.t * 'a) * PrivateTypes.t) list ->
  t ->
  (Ident.Name.t * 'a) list
