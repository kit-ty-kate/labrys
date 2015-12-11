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

type t = private
  { values : PrivateTypes.t GammaMap.Value.t
  ; variants : (int * PrivateTypes.t * int) GammaMap.Variant.t
  ; types : PrivateTypes.visibility GammaMap.Types.t
  ; type_vars : Kinds.t GammaMap.TypeVar.t
  ; constructors : (Ident.TypeVar.t list * (PrivateTypes.t list * int) GammaMap.Index.t) GammaMap.Constr.t
  ; exceptions : PrivateTypes.t list GammaMap.Exn.t
  ; tyclasses : PrivateTypes.class_t GammaMap.TyClass.t
  ; named_instances : (Ident.TyClass.t * PrivateTypes.t list) GammaMap.Instance.t
  }

val empty : t

val add_value : Ident.Name.t -> PrivateTypes.t -> t -> t
val add_variant : Ident.Variant.t -> (int * PrivateTypes.t * int) -> t -> t
val add_type : Ident.Type.t -> PrivateTypes.visibility -> t -> t
val add_type_var : Ident.TypeVar.t -> Kinds.t -> t -> t
val add_constr : Ident.Type.t -> Ident.Variant.t -> Ident.TypeVar.t list -> (PrivateTypes.t list * int) -> t -> t
val add_exception : Ident.Exn.t -> PrivateTypes.t list -> t -> t
val add_tyclass : Ident.TyClass.t -> PrivateTypes.class_t -> t -> t
val add_named_instance : Ident.Instance.t -> (Ident.TyClass.t * PrivateTypes.t list) -> t -> t

val replace_tyclass : Ident.TyClass.t -> PrivateTypes.class_t -> t -> t

val union : t -> t -> t

val is_subset_of : t -> t -> string list
