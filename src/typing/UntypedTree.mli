(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type name = Ident.Name.t
type ty_size = int
type pattern_var = (int list * name)
type index = int
type arity = int
type length = int

type ('int, 'float, 'char, 'string) ty = [
  | `Int of 'int
  | `Float of 'float
  | `Char of 'char
  | `String of 'string
]

type tag_ty = [(unit, unit, unit, unit) ty | `Custom]
type ret_ty = [tag_ty | `Void]
type const = (int, float, Uchar.t, string) ty

(* TODO: Improve *)
type 'a pattern' =
  | Node of (int option * ('a * 'a pattern') list)
  | Leaf of int

type ('a, 'b) rep = Index of 'a | Exn of 'b

type constr_rep = (index, name) rep
type pattern = (index pattern', name pattern') rep

type t =
  | Abs of (name * t)
  | App of (t * t)
  | Val of name
  | Var of (constr_rep * length)
  | PatternMatching of (t * (pattern_var list * t) list * t * pattern)
  | Let of (name * t * t)
  | LetRec of (name * t * t)
  | Fail of t
  | Try of (t * ((name * name list) * t) list)
  | RecordGet of (t * int)
  | RecordCreate of t list
  | Const of const
  | Unreachable

type foreign_fun_type = (ret_ty * tag_ty list)

type top =
  | Value of (name * t)
  | Foreign of (string * name * foreign_fun_type)
  | Exception of name
  | Instance of (name * (name * t) list)
