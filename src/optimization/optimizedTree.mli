(*
Copyright (c) 2013-2016 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

type name = LIdent.t
type eff_name = Ident.Exn.t
type free_vars = GammaSet.IDValue.t
type index = int
type constr = int
type arity = int
type length = int

type ('int, 'float, 'char, 'string) ty =
  ('int, 'float, 'char, 'string) LambdaTree.ty =
  | Int of 'int
  | Float of 'float
  | Char of 'char
  | String of 'string

type tag_ty = (unit, unit, unit, unit) ty
type const = (int, float, int, string) ty

type 'a tree' = 'a LambdaTree.tree' =
  | Node of (int option * ('a * 'a tree') list)
  | Leaf of int

type tree = LambdaTree.tree =
  | IdxTree of constr tree'
  | PtrTree of eff_name tree'

type foreign_ret_type = LambdaTree.foreign_ret_type =
  | Void
  | Alloc of tag_ty

type t =
  | Abs of (name * free_vars * t)
  | Rec of (name * t)
  | App of (name * name)
  | Val of name
  | Datatype of (index option * name list)
  | CallForeign of (string * foreign_ret_type * (tag_ty * name) list)
  | PatternMatching of (name * t list * t * tree)
  | Let of (name * t * t)
  | Fail of (eff_name * name list)
  | Try of (t * (name * t))
  | RecordGet of (name * index)
  | Const of const
  | Unreachable
  | Reraise of name

type linkage = LambdaTree.linkage = Public | Private

type top =
  | Value of (name * t * linkage)
  | Exception of eff_name
  | Function of (name * (name * t) * linkage)
