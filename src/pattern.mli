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

module Matrix : sig
  type 'a t

  val create :
    loc:ParseTree.location ->
    [`Alias of (Types.t * Kinds.t) | `Abstract of Kinds.t] Gamma.Types.t -> (* TODO: define types in a specific module *)
    TypesBeta.t Gamma.Constr.t -> (* TODO: Shouldn't be the same module as the other create *)
    TypesBeta.t ->
    'a ->
    ParseTree.pattern ->
    'a t

  val append :
    loc:ParseTree.location ->
    [`Alias of (Types.t * Kinds.t) | `Abstract of Kinds.t] Gamma.Types.t ->
    TypesBeta.t Gamma.Constr.t ->
    TypesBeta.t ->
    'a ->
    ParseTree.pattern ->
    'a t ->
    'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val get_results : 'a t -> 'a list
end

type constr =
  | Constr of string
  | Any of string

type var =
  | VLeaf
  | VNode of (int * var)

type t =
  | Node of (var * (constr * t) list)
  | Leaf of int

val create : string list Gamma.Constr.t -> 'a Matrix.t -> t
