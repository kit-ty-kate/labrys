(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

val check :
  current_module:Module.t ->
  interface:TypedEnv.env ->
  <lib_dir : string; with_main : bool; ..> ->
  TypedEnv.env ->
  PretypedTree.top list ->
  UntypedTree.top list

val check_interface :
  TypedEnv.env -> PretypedTree.interface list -> TypedEnv.env
