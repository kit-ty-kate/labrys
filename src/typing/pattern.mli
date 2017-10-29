(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type ty = TypedEnv.nty
type loc = Location.t
type name = Ident.Name.t
type constr_name = Ident.Constr.t
type index = int
type branch = int

(** Source language (typed) *)
type pattern' =
  | Wildcard
  | Constr of (loc * constr_name * pattern list)
  | Or of (pattern * pattern)
  | As of (pattern * name)
and pattern = (ty * pattern')

(** Target language (typed) *)
type tree =
  | Switch of ((loc * constr_name * ty * tree) list * tree option)
  | Swap of (index * tree)
  | Alias of (name * tree)
  | Jump of branch

type matrix = (pattern list * branch) list

val compile : matrix -> tree
