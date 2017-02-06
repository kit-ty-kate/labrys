(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type t = PrivateTypes.effects

val empty : t

val of_list :
  <lib_dir : string; ..> ->
  Gamma.t ->
  DesugaredTree.effects ->
  t

val is_empty : t -> bool

val equal : (Ident.TypeVar.t * Ident.TypeVar.t) list -> t -> t -> bool

val is_subset_of : (Ident.TypeVar.t * Ident.TypeVar.t) list -> t -> t -> bool

val has_io : <lib_dir : string; ..> -> t -> bool

val add_exn : Ident.Exn.t -> t -> t

val union : t -> t -> t

val union3 : t -> t -> t -> t

val union5 : t -> t -> t -> t -> t -> t

val remove_exn : Ident.Exn.t -> t -> t

val to_string : t -> string

val replace : from:Ident.TypeVar.t -> ty:PrivateTypes.t -> t -> t

val match_tyclass :
  is_tyclass:(Ident.TypeVar.t -> bool) ->
  is_tyclass_x:(Ident.TypeVar.t -> bool) ->
  t ->
  eff_x:t ->
  ((Ident.TypeVar.t * PrivateTypes.t) list * t * (Ident.TypeVar.t * PrivateTypes.t) list * t)

val unify_tyclass :
  is_new_tyvar:(Ident.TypeVar.t -> bool) ->
  t ->
  eff_x:t ->
  (Ident.TypeVar.t * PrivateTypes.t) list (* TODO: replace by a Set *)

val contains_free_tyvars : GammaSet.TypeVar.t -> t -> bool
