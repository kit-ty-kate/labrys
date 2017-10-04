(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

val kind_equal : PretypedTree.kind -> PretypedTree.kind -> bool

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

val check_eff :
  pure_arrow:[`Partial | `Forbid | `Allow] ->
  TypedEnv.env ->
  PretypedTree.ty ->
  TypedEnv.ty

val equal : TypedEnv.ty -> TypedEnv.ty -> bool
val app : TypedEnv.ty -> TypedEnv.ty -> TypedEnv.ty
val replace : Ident.Type.t -> by:TypedEnv.ty -> TypedEnv.ty -> TypedEnv.ty

val dump : TypedEnv.ty -> PPrint.document
val dump_eff : TypedEnv.effects -> PPrint.document
