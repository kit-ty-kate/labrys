(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type name = LIdent.t
type eff_name = Ident.Exn.t
type free_vars = GammaSet.MIDValue.t
type index = int
type constr = int
type arity = int
type length = int

type ('int, 'float, 'char, 'string) ty =
  ('int, 'float, 'char, 'string) FlattenTree.ty =
  | Int of 'int
  | Float of 'float
  | Char of 'char
  | String of 'string

type tag_ty = (unit, unit, unit, unit) ty
type const = (int, float, Uchar.t, string) ty

type 'a tree' = 'a FlattenTree.tree' =
  | Node of (int option * ('a * 'a tree') list)
  | Leaf of int

type tree = FlattenTree.tree =
  | IdxTree of constr tree'
  | PtrTree of eff_name tree'

type foreign_ret_type = FlattenTree.foreign_ret_type =
  | Void
  | Alloc of tag_ty

type t' =
  | Abs of (name * free_vars * t)
  | Rec of (name * t')
  | App of (name * name)
  | Val of name
  | Datatype of (index option * name list)
  | CallForeign of (string * foreign_ret_type * (tag_ty * name) list)
  | PatternMatching of (name * t list * t * tree)
  | Fail of (eff_name * name list)
  | Try of (t * (name * t))
  | RecordGet of (name * index)
  | Const of const
  | Unreachable
  | Reraise of name

and t = ((name * t') list * t')

type linkage = FlattenTree.linkage = Public | Private

type top =
  | Value of (name * t * linkage)
  | Exception of eff_name
  | Function of (name * (name * t) * linkage)
