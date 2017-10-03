(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

val check :
  pure_arrow:[`Partial | `Forbid | `Allow] ->
  TypedEnv.env ->
  PretypedTree.ty ->
  (TypedEnv.ty * TypedEnv.kind)

val check_value :
  pure_arrow:[`Partial | `Forbid | `Allow] ->
  TypedEnv.env ->
  PretypedTree.ty ->
  TypedEnv.ty

val is_subset_of : TypedEnv.ty -> TypedEnv.ty -> bool
val aty_is_subset_of : TypedEnv.aty -> TypedEnv.aty -> bool
val is_subset_of_list : TypedEnv.ty list -> TypedEnv.ty list -> bool

val dump : TypedEnv.ty -> PPrint.document
val dump_aty : Ident.Type.t -> TypedEnv.aty -> PPrint.document
val dump_exn : Ident.Exn.t -> TypedEnv.ty list -> PPrint.document
