(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

val pretype : DesugaredTree.top list -> PretypedTree.top list

val pretype_interface :
  DesugaredTree.interface list -> PretypedTree.interface list
