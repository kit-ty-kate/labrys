(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

(* TODO: Replace external uses of dump_kind by PretypedTreePrinter.dump_kind *)
val dump_kind : ParseTree.kind -> PPrint.document
val dump : ParseTree.top list -> PPrint.document
