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
type t_name = Ident.Type.t
type module_name = Ident.Module.t
type loc = Location.t

type t_value = (t_name * Kinds.t)

type ty =
  | Fun of (ty * ty)
  | Ty of t_name
  | Forall of (t_value * ty)
  | AbsOnTy of (t_value * ty)
  | AppOnTy of (ty * ty)

type value = (name * ty)

type pattern =
  | TyConstr of (name * pattern_arg list)
  | Any of name

and pattern_arg =
  | PVal of pattern
  | PTy of ty

type t =
  | Abs of (loc * value * t)
  | TAbs of (loc * t_value * t)
  | App of (loc * t * t)
  | TApp of (loc * t * ty)
  | Val of (loc * name)
  | PatternMatching of (loc * t * ((loc * pattern) * (loc * t)) list)
  | Let of (name * t * t)
  | LetRec of (loc * name * ty * t * t)

type variant =
  | Variant of (loc * name * ty)

type datatype = (loc * t_name * Kinds.t * variant list)

type typeAlias = (loc * t_name * ty)

type top =
  | Value of (name * t)
  | RecValue of (loc * name * ty * t)
  | Type of typeAlias
  | Binding of (loc * name * ty * string)
  | Datatype of datatype

type imports = module_name list
