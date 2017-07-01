(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

val pretype :
  <lib_dir : string; ..> ->
  DesugaredTree.top list ->
  PretypedTree.top list
