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

type kind = ParseTree.kind

type ty =
  | Fun of (ty * ty)
  | Ty of string
  | Forall of (string * kind * ty)
  | AbsOnTy of (string * kind * ty)
  | AppOnTy of (ty * ty)

type t = (ty * kind)

type env = (string * t)

val get_kind : kind option -> kind
val kind_equal : kind -> kind -> bool

val to_string : ty -> string
val from_parse_tree : loc:ParseTree.location -> env list -> ParseTree.ty -> t
val equal : env list -> ty -> ty -> bool

val replace : from:string -> ty:ty -> ty -> ty

val size : ty -> int
