(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

val check :
  modul:Module.t ->
  interface:Gamma.t ->
  with_main:bool ->
  <lib_dir : string; ..> ->
  Gamma.t ->
  PretypedTree.top list ->
  UntypedTree.top list
