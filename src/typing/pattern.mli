(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type name = Ident.Name.t
type eff_name = Ident.Exn.t
type variant_name = Ident.Variant.t

type index = int

type constr = (variant_name * index)

type 'a t' =
  | Node of (int option * ('a * 'a t') list)
  | Leaf of int

type t =
  | Idx of constr t'
  | Ptr of eff_name t'

val create :
  loc:Location.t ->
  (Env.t -> PretypedTree.t -> ('a * Types.t * Effects.t)) ->
  Env.t ->
  Types.t ->
  (PretypedTree.pattern * PretypedTree.t) list ->
  (t * ((PatternMatrix.var * name) list * 'a) list * Types.t * Effects.t)
