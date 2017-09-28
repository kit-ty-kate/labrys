(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

val check :
  modul:Module.t ->
  interface:TypedEnv.env ->
  with_main:bool ->
  <lib_dir : string; ..> ->
  TypedEnv.env ->
  PretypedTree.top list ->
  UntypedTree.top list

val check_interface :
  current_module:Module.t ->
  <lib_dir : string; ..> ->
  TypedEnv.env ->
  PretypedTree.interface list ->
  TypedEnv.env
