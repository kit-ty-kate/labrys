(*
Copyright (c) 2013 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

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

type t

type visibility =
  | Abstract of Kinds.t
  | Alias of (t * Kinds.t)

val of_parse_tree_kind :
  visibility GammaMap.Types.t ->
  ParseTree.ty ->
  (t * Kinds.t)

val of_parse_tree :
  visibility GammaMap.Types.t ->
  ParseTree.ty ->
  t

val func : param:t -> eff:Effects.t -> res:t -> t
val forall : param:name -> kind:Kinds.t -> res:t -> t

val to_string : t -> string

val equal : t -> t -> bool

val replace : from:name -> ty:t -> t -> t

val size : t -> int

val head : t -> name

module Error : sig
  val fail : loc:Location.t -> has:t -> expected:t -> 'a

  val fail_return_type : loc:Location.t -> Ident.Name.t -> 'a
end

val apply :
  loc:Location.t ->
  t ->
  (t * Effects.t * t)

val apply_ty :
  loc:Location.t ->
  ty_x:t ->
  kind_x:Kinds.t ->
  t ->
  (name * t)

val check_if_returns_type : datatype:name -> t -> bool
