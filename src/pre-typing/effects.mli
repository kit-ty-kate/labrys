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

type t = PrivateTypes.effects

val empty : t

val of_list :
  <lib_dir : string; ..> ->
  Gamma.t ->
  DesugaredTree.effects ->
  t

val is_empty : t -> bool

val equal : (Ident.TypeVar.t * Ident.TypeVar.t) list -> t -> t -> bool

val is_subset_of : (Ident.TypeVar.t * Ident.TypeVar.t) list -> t -> t -> bool

val has_io : <lib_dir : string; ..> -> t -> bool

val add_exn : Ident.Exn.t -> t -> t

val union : t -> t -> t

val union3 : t -> t -> t -> t

val union5 : t -> t -> t -> t -> t -> t

val remove_exn : Ident.Exn.t -> t -> t

val to_string : t -> string

val replace : from:Ident.TypeVar.t -> ty:PrivateTypes.t -> t -> t

val match_tyclass :
  is_tyclass:(Ident.TypeVar.t -> bool) ->
  is_tyclass_x:(Ident.TypeVar.t -> bool) ->
  t ->
  eff_x:t ->
  ((Ident.TypeVar.t * PrivateTypes.t) list * t * (Ident.TypeVar.t * PrivateTypes.t) list * t)

val unify_tyclass :
  is_new_tyvar:(Ident.TypeVar.t -> bool) ->
  t ->
  eff_x:t ->
  (Ident.TypeVar.t * PrivateTypes.t) list (* TODO: replace by a Set *)

val contains_free_tyvars : GammaSet.TypeVar.t -> t -> bool
