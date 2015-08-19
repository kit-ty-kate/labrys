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
  UnsugaredTree.effects ->
  t

val is_empty : t -> bool

val equal : (Ident.Type.t * Ident.Type.t) list -> t -> t -> bool

val is_subset_of : (Ident.Type.t * Ident.Type.t) list -> t -> t -> bool

val has_io : t -> bool

val add_exn : Ident.Exn.t -> t -> t

val union : t -> t -> t

val union3 : t -> t -> t -> t

val remove_exn : Ident.Exn.t -> t -> t

val to_string : t -> string

val replace : from:Ident.Type.t -> ty:PrivateTypes.t -> t -> t

val remove_module_aliases : Ident.Type.t list -> t -> t

val match_tyclass :
  is_tyclass:(Ident.Type.t -> bool) ->
  t ->
  eff_x:t ->
  ((Ident.Type.t * PrivateTypes.t) list * t)
