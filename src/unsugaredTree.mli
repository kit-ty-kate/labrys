(*
Copyright (c) 2013 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

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
type module_name = Ident.Module.t
type loc = Location.t

type t_value = (t_name * Kinds.t)

type eff = (Ident.Eff.t * Ident.Exn.t list)

type is_rec = ParseTree.is_rec =
  | Rec
  | NonRec

type ty' =
  | Fun of (ty * eff list * ty)
  | Ty of t_name
  | Forall of (t_value * ty)
  | AbsOnTy of (t_value * ty)
  | AppOnTy of (ty * ty)

and ty = (loc * ty')

type ty_annot = (ty * eff list) option
type value = (name * ty)

type pattern =
  | TyConstr of (loc * name * pattern_arg list)
  | Any of name

and pattern_arg =
  | PVal of pattern
  | PTy of ty

type t' =
  | Abs of (value * t)
  | TAbs of (t_value * t)
  | App of (t * t)
  | TApp of (t * ty)
  | Val of name
  | PatternMatching of (t * (pattern * t) list)
  | Let of ((name * is_rec * (ty_annot * t)) * t)
  | Fail of (ty * (exn_name * t list))
  | Try of (t * ((exn_name * name list) * t) list)

and t = (loc * t')

type variant = Variant of (loc * name * ty)

type top' =
  | Value of (name * is_rec * (ty_annot * t))
  | Type of (t_name * ty)
  | Binding of (name * ty * string)
  | Datatype of (t_name * Kinds.t * variant list)
  | Exception of (exn_name * ty list)

and top = (loc * top')

type imports = module_name list
