(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type name = LIdent.t
type free_vars = EnvSet.MIDValue.t
type index = int
type constr = int
type arity = int
type length = int

type ('int, 'float, 'char, 'string) ty =
  ('int, 'float, 'char, 'string) FlattenTree.ty

type tag_ty = [(unit, unit, unit, unit) ty | `Custom]
type ret_ty = [tag_ty | `Void]
type const = (int, float, Uchar.t, string) ty

type 'a tree' = 'a FlattenTree.tree' =
  | Node of (int option * ('a * 'a tree') list)
  | Leaf of int

type ('a, 'b) rep = ('a, 'b) FlattenTree.rep = Index of 'a | Exn of 'b

type constr_rep = (constr, name) rep
type tree = (constr tree', name tree') rep

type t' =
  | Abs of (name * free_vars * t)
  | Rec of (name * t')
  | App of (name * name)
  | Val of name
  | Datatype of (constr_rep option * name list)
  | CallForeign of (string * ret_ty * (tag_ty * name) list)
  | PatternMatching of (name * t list * t * tree)
  | Fail of name
  | Try of (t * (name * t))
  | RecordGet of (name * index)
  | Const of const
  | Unreachable

and t = ((name * t') list * t')

type linkage = FlattenTree.linkage = Public | Private

type top =
  | Value of (name * t * linkage)
  | Exception of name
  | Function of (name * (name * t) * linkage)
