(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type name = Ident.Name.t
type constr_name = Ident.Constr.t
type exn_name = Ident.Exn.t
type t_name = Ident.Type.t
type tyvar_name = Ident.TyVar.t
type tyclass_name = Ident.TyClass.t
type instance_name = Ident.Instance.t
type module_name = Module.t
type loc = Location.t

type kind = ParseTree.kind =
  | KStar
  | KEff
  | KFun of (kind * kind)

type t_value = (tyvar_name * kind)

type tyclass = (tyclass_name * t_value list * ty list)

and effects = (loc * ty list)

and ty' =
  | Fun of (ty * effects option * ty)
  | Ty of t_name
  | TyVar of tyvar_name
  | Eff of effects
  | Forall of (t_value * ty)
  | TyClass of (tyclass * effects option * ty)
  | AbsOnTy of (t_value * ty)
  | AppOnTy of (ty * ty)

and ty = (loc * ty')

type ty_annot = (ty * ty option)
type tyclass_instance = (tyclass_name * ty list)

type tyclass_app_arg =
  | TyClassVariable of instance_name
  | TyClassInstance of tyclass_instance

type pattern =
  | TyConstr of (loc * constr_name * pattern list)
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
  | Var of constr_name
  | PatternMatching of (t * (pattern * t) list)
  | Let of (name * t * t)
  | LetRec of (name * t * t)
  | Fail of (ty * (exn_name * t list))
  | Try of (t * ((exn_name * name list) * t) list)
  | Annot of (t * ty_annot)
  | Const of const

and t = (loc * t')

type variant = (constr_name * ty list * ty)

type top =
  | Value of (name * t)
  | Type of (t_name * ty)
  | Foreign of (string * name * ty)
  | Datatype of (t_name * kind * (tyvar_name * kind) list * variant list)
  | Exception of (exn_name * ty list)
  | Class of (tyclass_name * t_value list * (name * ty) list)
  | Instance of (tyclass_instance * instance_name option * (name * t) list)

type imports = module_name list

type interface =
  | IVal of (name * ty)
  | IAbstractType of (t_name * kind)
  | IDatatype of (t_name * kind * (tyvar_name * kind) list * variant list)
  | ITypeAlias of (t_name * ty)
  | IException of (exn_name * ty list)
  | IClass of (tyclass_name * (tyvar_name * kind) list * (name * ty) list)
  | IInstance of (tyclass_instance * instance_name option)
