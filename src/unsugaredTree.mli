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
type variant_name = Ident.Variant.t
type exn_name = Ident.Exn.t
type t_name = Ident.Type.t
type tyclass_name = Ident.TyClass.t
type instance_name = Ident.Instance.t
type module_name = Module.t
type loc = Location.t

type t_value = (t_name * Kinds.t)

type effects = (loc * (t_name * exn_name list) list)

type is_rec = ParseTree.is_rec =
  | Rec
  | NonRec

type tyclass_value = (tyclass_name * tyclass_arg list)

and tyclass_arg =
  | Param of t_name
  | Filled of ty

and ty' =
  | Fun of (ty * effects option * ty)
  | Ty of t_name
  | Eff of effects
  | Forall of (t_value * ty)
  | TyClass of (tyclass_value * effects option * ty)
  | AbsOnTy of (t_value * ty)
  | AppOnTy of (ty * ty)

and ty = (loc * ty')

type ty_annot = (ty * effects option)

type tyclass_instance = (tyclass_name * ty list)

type tyclass_app_arg =
  | TyClassVariable of instance_name
  | TyClassInstance of tyclass_instance

type pattern =
  | TyConstr of (loc * variant_name * pattern list)
  | Any of name

type const =
  | Int of int
  | Float of float
  | Char of int
  | String of string

type value = (name * is_rec * t)

and t' =
  | Abs of ((name * ty) * t)
  | TAbs of (t_value * t)
  | CAbs of ((instance_name * tyclass_value) * t)
  | App of (t * t)
  | TApp of (t * ty)
  | CApp of (t * tyclass_app_arg)
  | Val of name
  | Var of variant_name
  | PatternMatching of (t * (pattern * t) list)
  | Let of (value * t)
  | Fail of (ty * (exn_name * t list))
  | Try of (t * ((exn_name * name list) * t) list)
  | Annot of (t * ty_annot)
  | Const of const

and t = (loc * t')

type variant = Variant of (variant_name * ty list * ty)

type top =
  | Value of value
  | Type of (t_name * ty)
  | Binding of (name * ty * string)
  | Datatype of (t_name * Kinds.t * (t_name * Kinds.t) list * variant list)
  | Exception of (exn_name * ty list)
  | Open of module_name
  | Class of (tyclass_name * t_value list * (name * ty) list)
  | Instance of (tyclass_instance * instance_name option * value list)

type imports = module_name list
