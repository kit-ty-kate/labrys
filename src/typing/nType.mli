(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

val check :
  pure_arrow:[`Partial | `Forbid | `Allow] ->
  TypedEnv.env ->
  PretypedTree.ty ->
  TypedEnv.nty

val check_eff :
  pure_arrow:[`Partial | `Forbid | `Allow] ->
  TypedEnv.env ->
  PretypedTree.ty ->
  TypedEnv.neffects

val is_subset_of : TypedEnv.nty -> TypedEnv.nty -> bool
val eff_is_subset_of : TypedEnv.neffects -> TypedEnv.neffects -> bool
val aty_is_subset_of : TypedEnv.aty -> TypedEnv.aty -> bool
val is_subset_of_list : TypedEnv.nty list -> TypedEnv.nty list -> bool

val size : TypedEnv.nty -> int

val dump : TypedEnv.nty -> PPrint.document
val dump_eff : TypedEnv.neffects -> PPrint.document
val dump_aty : Ident.Type.t -> TypedEnv.aty -> PPrint.document
val dump_exn : Ident.Exn.t -> TypedEnv.nty list -> PPrint.document
