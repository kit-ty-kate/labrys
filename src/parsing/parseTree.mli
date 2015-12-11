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

type loc = Location.t

type new_lower_name = (loc * [`NewLowerName of string | `Underscore])
type new_upper_name = (loc * [`NewUpperName of string])

type lower_name = (loc * [`LowerName of string list])
type upper_name = (loc * [`UpperName of string list])

type ty_arg = (new_lower_name * Kinds.t option)

type effect_name =
  | EffTy of (upper_name * upper_name list)
  | EffTyVar of new_lower_name

type effects = (loc * effect_name list)

type is_rec =
  | Rec
  | NonRec

and tyclass = (upper_name * ty_arg list * ty list)

and ty' =
  | Fun of (ty * effects option * ty)
  | Ty of upper_name
  | TyVar of new_lower_name
  | Eff of effects
  | Forall of (ty_arg list * ty)
  | TyClass of (tyclass * effects option * ty)
  | AbsOnTy of (ty_arg list * ty)
  | AppOnTy of (ty * ty)

and ty = (loc * ty')

type ty_annot = (ty * effects option)
type v_arg = (new_lower_name * ty)

type pattern =
  | TyConstr of (loc * upper_name * pattern list)
  | Any of new_lower_name

type arg' =
  | VArg of v_arg
  | TArg of ty_arg
  | Unit
  | TyClassArg of (new_lower_name * tyclass)

and arg = (loc * arg')

type tyclass_instance = (upper_name * ty list)

type tyclass_app_arg =
  | TyClassVariable of lower_name
  | TyClassInstance of tyclass_instance

type const =
  | Int of string
  | Float of string
  | Char of char list
  | String of char list

type value = (new_lower_name * is_rec * (arg list * (ty_annot option * t)))

and t' =
  | Abs of (arg list * (ty_annot option * t))
  | App of (t * t)
  | TApp of (t * ty)
  | TyClassApp of (t * tyclass_app_arg)
  | LowerVal of lower_name
  | UpperVal of upper_name
  | PatternMatching of (t * (pattern * t) list)
  | Let of (value * t)
  | Fail of (ty * (upper_name * t list))
  | Try of (t * ((upper_name * new_lower_name list) * t) list)
  | Seq of (t * t)
  | Annot of (t * ty_annot)
  | Const of const

and t = (loc * t')

type variant = Variant of (new_upper_name * ty list)

type import =
  | Source of upper_name
  | Library of upper_name

type top =
  | Value of value
  | Type of (new_upper_name * ty)
  | AbstractType of (new_upper_name * Kinds.t option)
  | Foreign of (char list * new_lower_name * ty)
  | Datatype of (new_upper_name * ty_arg list * variant list)
  | Exception of (new_upper_name * ty list)
  | Open of import
  | Class of (new_upper_name * ty_arg list * (new_lower_name * ty) list)
  | Instance of (tyclass_instance * new_lower_name option * value list)

type imports = import list

type interface =
  | IVal of (new_lower_name * ty)
  | IAbstractType of (new_upper_name * Kinds.t option)
  | IDatatype of (new_upper_name * ty_arg list * variant list)
  | ITypeAlias of (new_upper_name * ty)
  | IException of (new_upper_name * ty list)
  | IOpen of import
  | IClass of (new_upper_name * ty_arg list * (new_lower_name * ty) list)
  | IInstance of (tyclass_instance * new_lower_name option)
