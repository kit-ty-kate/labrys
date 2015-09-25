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

type name = Ident.Type.t
type tyvar_name = Ident.TypeVar.t

type t = PrivateTypes.t

type visibility = PrivateTypes.visibility =
  | Abstract of Kinds.t
  | Alias of (t * Kinds.t)

val of_parse_tree_kind :
  pure_arrow:[< `Allow | `Partial | `Forbid] ->
  <lib_dir : string; ..> ->
  Gamma.t ->
  UnsugaredTree.ty ->
  (t * Kinds.t)

val of_parse_tree :
  pure_arrow:[< `Allow | `Partial | `Forbid] ->
  <lib_dir : string; ..> ->
  Gamma.t ->
  UnsugaredTree.ty ->
  t

val to_string : t -> string

val equal : t -> t -> bool

val is_subset_of : t -> t -> bool

val replace : from:tyvar_name -> ty:t -> t -> t

val is_value : t -> bool

val size : t -> int

val head : t -> (name * t list)

module TyErr : sig
  val fail : loc_t:Location.t -> has:t -> expected:t -> 'a
end

val apply :
  loc_f:Location.t ->
  loc_x:Location.t ->
  t ->
  t ->
  (Effects.t * t * t)

val apply_ty :
  loc_f:Location.t ->
  loc_x:Location.t ->
  ty_x:t ->
  kind_x:Kinds.t ->
  t ->
  t

val apply_tyclass : t -> Ident.TyClass.t -> PrivateTypes.tyclass_arg list -> (t * Effects.t)

val has_io : <lib_dir : string; ..> -> t -> bool

val is_fun : t -> bool

val is_unit : <lib_dir : string; ..> -> t -> bool

val remove_module_aliases : t -> t

val tyclass_wrap : Ident.TyClass.t -> (tyvar_name * Kinds.t) list -> t -> t

val extract_filled_tyclasses :
  t ->
  ((Ident.TyClass.t * t list) option list * Effects.t * t)

val forall : tyvar_name * Kinds.t * t -> t
val tyclass : (Ident.TyClass.t * PrivateTypes.tyclass_arg list) * PrivateTypes.effects * t -> t
