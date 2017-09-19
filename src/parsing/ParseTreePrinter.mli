(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

val dump_kind : ParseTree.kind -> PPrint.document
val dump : ParseTree.top list -> PPrint.document
