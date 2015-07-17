(*
Copyright (c) 2013-2015 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

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

type name = Ident.Name.t
type exn_name = Ident.Exn.t
type t_name = Ident.Type.t
type tyclass_name = Ident.TyClass.t
type module_name = Module.t
type loc = Location.t

type t_value = (t_name * Kinds.t)
type tyclass_value = (tyclass_name * t_name list)

type effects = (loc * (t_name * exn_name list) list)

type is_rec = ParseTree.is_rec =
  | Rec
  | NonRec

type ty' =
  | Fun of (ty * effects option * ty)
  | Ty of t_name
  | Eff of effects
  | Forall of (t_value * ty)
  | ForallTyClass of (tyclass_value * ty)
  | AbsOnTy of (t_value * ty)
  | AppOnTy of (ty * ty)

and ty = (loc * ty')

type ty_annot = (ty * effects option)
type value = (name * ty)

type pattern =
  | TyConstr of (loc * name * pattern list)
  | Any of name

type t' =
  | Abs of (value * t)
  | TAbs of (t_value * t)
  | App of (t * t)
  | TApp of (t * ty)
  | Val of name
  | PatternMatching of (t * (pattern * t) list)
  | Let of ((name * is_rec * t) * t)
  | Fail of (ty * (exn_name * t list))
  | Try of (t * ((exn_name * name list) * t) list)
  | Annot of (t * ty_annot)

and t = (loc * t')

type variant = Variant of (name * ty list * ty)

type top =
  | Value of (name * is_rec * t)
  | Type of (t_name * ty)
  | Binding of (name * ty * string)
  | Datatype of (t_name * Kinds.t * (t_name * Kinds.t) list * variant list)
  | Exception of (exn_name * ty list)
  | Open of module_name

type imports = module_name list
