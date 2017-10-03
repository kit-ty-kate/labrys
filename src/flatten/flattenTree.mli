(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type name = LIdent.t
type eff_name = Ident.Exn.t
type index = int
type constr = int
type arity = int
type length = int

type ('int, 'float, 'char, 'string) ty =
  ('int, 'float, 'char, 'string) LambdaTree.ty

type tag_ty = [(unit, unit, unit, unit) ty | `Custom]
type ret_ty = [tag_ty | `Void]
type const = (int, float, Uchar.t, string) ty

type 'a tree' = 'a LambdaTree.tree' =
  | Node of (int option * ('a * 'a tree') list)
  | Leaf of int

type tree = LambdaTree.tree =
  | IdxTree of constr tree'
  | PtrTree of eff_name tree'

type t' =
  | Abs of (name * t)
  | Rec of (name * t') (* TODO: Remove this case (LIdent should be sufficiant) *)
  | App of (name * name)
  | Val of name
  | Datatype of (index option * name list)
  | CallForeign of (string * ret_ty * (tag_ty * name) list)
  | PatternMatching of (name * t list * t * tree)
  | Fail of (eff_name * name list)
  | Try of (t * (name * t))
  | RecordGet of (name * index)
  | Const of const
  | Unreachable
  | Reraise of name

and t = ((name * t') list * t')

type linkage = LambdaTree.linkage = Public | Private

type top =
  | Value of (name * t * linkage)
  | Exception of eff_name
