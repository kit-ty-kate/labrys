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

module Exn_set : Utils.EQSET with type elt = Ident.Exn.t
module Variables : Utils.EQSET with type elt = Ident.Type.t

type name = Ident.Name.t
type ty_name = Ident.Type.t

type effects =
  { variables : Variables.t
  ; exns : Exn_set.t
  }

type tyclass_arg =
  | Param of (ty_name * Kinds.t)
  | Filled of t

and t =
  | Ty of ty_name
  | Eff of effects
  | Fun of (t * effects * t)
  | Forall of (ty_name * Kinds.t * t)
  | TyClass of ((Ident.TyClass.t * tyclass_arg list) * effects * t)
  | AbsOnTy of (ty_name * Kinds.t * t)
  | AppOnTy of (t * t)

type visibility =
  | Abstract of Kinds.t
  | Alias of (t * Kinds.t)

val ty_empty : < lib_dir : string; .. > -> visibility GammaMap.Types.t

val ty_equal : t -> t -> bool

val ty_is_subset_of : t -> t -> bool

val ty_to_string : t -> string

val tyclass_args_remove_module_aliases : tyclass_arg list -> tyclass_arg list

val tyclass_args_equal : tyclass_arg list -> tyclass_arg list -> bool

val ty_remove_module_aliases : t -> t

val ty_reduce : t -> t

val ty_replace : from:Ident.Type.t -> ty:t -> t -> t

val eff_empty : effects

val eff_union : effects -> effects -> effects

val eff_union_ty : ?from:Ident.Type.t -> effects -> t -> effects

val eff_replace : from:Ident.Type.t -> ty:t -> effects -> effects

val eff_equal : (ty_name * ty_name) list -> effects -> effects -> bool

val eff_is_subset_of : (ty_name * ty_name) list -> effects -> effects -> bool

val eff_remove_module_aliases : ty_name list -> effects -> effects

val eff_is_empty : effects -> bool

val eff_to_string : effects -> string

module Instances : Utils.EQMAP with type key = t list

(* TODO: Handle contraints *)
type class_t =
  { params : (ty_name * Kinds.t) list
  ; signature : (name * t) list
  ; instances : name Instances.t
  }

val class_equal : class_t -> class_t -> bool

val class_remove_module_aliases : class_t -> class_t
