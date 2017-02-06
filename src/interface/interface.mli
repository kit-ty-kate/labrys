(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

val compile :
  current_module:Module.t ->
  <lib_dir : string; ..> ->
  Gamma.t ->
  InterfaceTree.t list ->
  Gamma.t
