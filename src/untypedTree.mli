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

type name = Ident.Name.t
type used_vars = name BatSet.t
type index = int
type with_exn = bool
type constr = int

type tree =
  | Node of (Pattern.var * (constr * tree) list)
  | Leaf of int

and t =
  | Abs of (name * with_exn * used_vars * t)
  | App of (t * with_exn * t)
  | Val of name
  | Variant of index
  | PatternMatching of (t * ((Pattern.var * name) list * t) list * tree)
  | Let of (name * t * t)
  | LetRec of (name * t * t)
  | Fail of name

type top =
  | Value of (name * t)
  | RecValue of (name * t)
  | Binding of (name * string)
  | Exception of name
