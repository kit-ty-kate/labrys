(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type loc = Location.t

type new_lower_name = (loc * [`NewLowerName of string | `Underscore])
type new_upper_name = (loc * [`NewUpperName of string])

type lower_name = (loc * [`LowerName of string list])
type upper_name = (loc * [`UpperName of string list])

type kind =
  | KStar
  | KEff
  | KExn
  | KFun of (kind * kind)

type ty_arg = (new_lower_name * kind option)

type is_rec =
  | Rec
  | NonRec

type tyclass = (upper_name * ty_arg list * ty list)

and effects = (loc * ty list)

and ty' =
  | Fun of (ty * effects option * ty)
  | Ty of upper_name
  | TyVar of new_lower_name
  | Eff of effects
  | Sum of ty list
  | Forall of (ty_arg list * ty)
  | TyClass of (tyclass * effects option * ty)
  | AbsOnTy of (ty_arg list * ty)
  | AppOnTy of (ty * ty)

and ty = (loc * ty')

type ty_annot = (ty * ty option)
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
  | Fail of (ty * t)
  | Try of (t * (pattern * t) list)
  | Seq of (t * t)
  | Annot of (t * ty_annot)
  | Const of const

and t = (loc * t')

type variant = (new_upper_name * ty list)

type import =
  | Source of upper_name
  | Library of upper_name

type top =
  | Value of value
  | Type of (new_upper_name * ty)
  | AbstractType of (new_upper_name * kind option)
  | Foreign of (char list * new_lower_name * ty)
  | Datatype of (new_upper_name * ty_arg list * variant list)
  | Exception of (new_upper_name * ty list)
  | Open of import
  | Class of (new_upper_name * ty_arg list * (new_lower_name * ty) list)
  | Instance of (tyclass_instance * new_lower_name option * value list)

type imports = import list

type interface =
  | IVal of (new_lower_name * ty)
  | IAbstractType of (new_upper_name * kind option)
  | IDatatype of (new_upper_name * ty_arg list * variant list)
  | ITypeAlias of (new_upper_name * ty)
  | IException of (new_upper_name * ty list)
  | IOpen of import
  | IClass of (new_upper_name * ty_arg list * (new_lower_name * ty) list)
  | IInstance of (tyclass_instance * new_lower_name option)
