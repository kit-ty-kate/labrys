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

type name = Ident.Type.t

type effects =
  { variables : Variables.t
  ; exns : Exn_set.t
  }

type tyclass_arg =
  | Param of (name * Kinds.t)
  | Filled of t

and t =
  | Ty of name
  | Eff of effects
  | Fun of (t * effects * t)
  | Forall of (name * Kinds.t * t)
  | TyClass of ((Ident.TyClass.t * tyclass_arg list) * effects * t)
  | AbsOnTy of (name * Kinds.t * t)
  | AppOnTy of (t * t)

type visibility =
  | Abstract of Kinds.t
  | Alias of (t * Kinds.t)

val ty_empty : < lib_dir : string; .. > -> visibility GammaMap.Types.t

val ty_equal : t -> t -> bool

val ty_is_subset_of : t -> t -> bool

val ty_remove_module_aliases : t -> t

val eff_equal : (name * name) list -> effects -> effects -> bool

val eff_is_subset_of : (name * name) list -> effects -> effects -> bool

val eff_remove_module_aliases : name list -> effects -> effects
