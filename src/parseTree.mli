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

type new_lower_name = [`NewLowerName of string | `Underscore]
type new_upper_name = [`NewUpperName of string]

type lower_name = [`LowerName of string list]
type upper_name = [`UpperName of string list]

type loc = Location.t

type ty_arg = (new_upper_name * Kinds.t option)

type forall_arg =
  | Eff of new_upper_name
  | Typ of ty_arg

type eff = (new_upper_name * new_upper_name list)

type is_rec =
  | Rec
  | NonRec

type ty' =
  | Fun of (ty * eff list option * ty)
  | Ty of upper_name
  | Forall of (forall_arg list * ty)
  | AbsOnTy of (ty_arg list * ty)
  | AppOnTy of (ty * ty)

and ty = (loc * ty')

type ty_annot = (ty * eff list option)
type v_arg = (new_lower_name * ty)

type pattern =
  | TyConstr of (loc * upper_name * pattern_arg list)
  | Any of new_lower_name

and pattern_arg =
  | PVal of pattern
  | PTy of ty

type arg' =
  | VArg of v_arg
  | TArg of ty_arg
  | EArg of new_upper_name
  | Unit

and arg = (loc * arg')

type t' =
  | Abs of (arg list * t)
  | App of (t * t)
  | TApp of (t * ty)
  | EApp of (t * eff list)
  | Val of [lower_name | upper_name]
  | PatternMatching of (t * (pattern * t) list)
  | Let of ((new_lower_name * is_rec * (arg list * (ty_annot option * t))) * t)
  | Fail of (ty * (upper_name * t list))
  | Try of (t * ((upper_name * new_lower_name list) * t) list)
  | Seq of (t * t)
  | Annot of (t * ty_annot)

and t = (loc * t')

type variant = Variant of (loc * new_upper_name * ty)

type top' =
  | Value of (new_lower_name * is_rec * (arg list * (ty_annot option * t)))
  | Type of (new_upper_name * ty)
  | Binding of (new_lower_name* ty * string)
  | Datatype of (new_upper_name * Kinds.t option * variant list)
  | Exception of (new_upper_name * ty list)

and top = (loc * top')

type imports = upper_name list

type interface' =
  | IVal of (new_lower_name * ty)
  | IAbstractType of (new_upper_name * Kinds.t option)
  | IDatatype of (new_upper_name * Kinds.t option * variant list)
  | ITypeAlias of (new_upper_name * ty)
  | IException of (new_upper_name * ty list)

type interface = (loc * interface')
