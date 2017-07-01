(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

val check :
  modul:Module.t ->
  interface:Env.t ->
  with_main:bool ->
  <lib_dir : string; ..> ->
  Env.t ->
  PretypedTree.top list ->
  UntypedTree.top list
