(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

module FlattenTree : sig
  val dump : FlattenTree.top list -> string
end

module OptimizedTree : sig
  val dump : OptimizedTree.top list -> string
end
