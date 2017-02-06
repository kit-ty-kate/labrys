(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

module ParseTree : sig
  val dump : ParseTree.top list -> string
end

module DesugaredTree : sig
  val dump : DesugaredTree.top list -> string
end

module UntypedTree : sig
  val dump : UntypedTree.top list -> string
end

module LambdaTree : sig
  val dump : LambdaTree.top list -> string
end

module FlattenTree : sig
  val dump : FlattenTree.top list -> string
end

module OptimizedTree : sig
  val dump : OptimizedTree.top list -> string
end
