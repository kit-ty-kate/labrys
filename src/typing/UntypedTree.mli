(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type name = Ident.Name.t
type eff_name = Ident.Exn.t
type ty_size = int
type pattern_var = (PatternMatrix.var * name)
type index = int
type arity = int
type length = int

type ('int, 'float, 'char, 'string) ty =
  | Int of 'int
  | Float of 'float
  | Char of 'char
  | String of 'string

type tag_ty = (unit, unit, unit, unit) ty
type const = (int, float, Uchar.t, string) ty

type foreign_ret_type =
  | Void
  | Alloc of tag_ty

type t =
  | Abs of (name * t)
  | App of (t * t)
  | Val of name
  | Var of (index * length)
  | PatternMatching of (t * (pattern_var list * t) list * t * Pattern.t)
  | Let of (name * t * t)
  | LetRec of (name * t * t)
  | Fail of (eff_name * t list)
  | Try of (t * (name * t))
  | RecordGet of (t * int)
  | RecordCreate of t list
  | Const of const
  | Unreachable
  | Reraise of name

type foreign_fun_type = (foreign_ret_type * tag_ty list)

type top =
  | Value of (name * t)
  | Foreign of (string * name * foreign_fun_type)
  | Exception of eff_name
