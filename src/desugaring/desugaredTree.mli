(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type name = Ident.Name.t
type variant_name = Ident.Variant.t
type exn_name = Ident.Exn.t
type t_name = Ident.Type.t
type tyvar_name = Ident.TypeVar.t
type tyclass_name = Ident.TyClass.t
type instance_name = Ident.Instance.t
type module_name = Module.t
type loc = Location.t

type t_value = (tyvar_name * Kinds.t)

type effect_name =
  | EffTy of (t_name * exn_name list)
  | EffTyVar of tyvar_name

type effects = (loc * effect_name list)

type tyclass = (tyclass_name * t_value list * ty list)

and ty' =
  | Ty of t_name
  | TyVar of tyvar_name
  | Eff of effects
  | Forall of (t_value * ty)
  | TyClass of (tyclass * effects * ty)
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
  | Char of Uchar.t
  | String of string

type t' =
  | Abs of ((name * ty) * t)
  | TAbs of (t_value * t)
  | CAbs of ((instance_name * tyclass) * t)
  | App of (t * t)
  | TApp of (t * ty)
  | CApp of (t * tyclass_app_arg)
  | Val of name
  | Var of variant_name
  | PatternMatching of (t * (pattern * t) list)
  | Let of (name * t * t)
  | LetRec of (name * t * t)
  | Fail of (ty * (exn_name * t list))
  | Try of (t * ((exn_name * name list) * t) list)
  | Annot of (t * ty_annot)
  | Const of const

and t = (loc * t')

type variant = (variant_name * ty list * ty)

type top =
  | Value of (name * t)
  | Type of (t_name * ty)
  | Foreign of (string * name * ty)
  | Datatype of (t_name * Kinds.t * (tyvar_name * Kinds.t) list * variant list)
  | Exception of (exn_name * ty list)
  | Class of (tyclass_name * t_value list * (name * ty) list)
  | Instance of (tyclass_instance * instance_name option * (name * t) list)

type imports = module_name list
