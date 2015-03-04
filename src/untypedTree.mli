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
type eff_name = Ident.Exn.t
type used_vars = name BatSet.t
type index = int
type constr = int
type arity = int

type tree =
  | Node of (Pattern.var * (constr * tree) list)
  | Leaf of int

and t =
  | Abs of (name * used_vars * t)
  | App of (t * t)
  | Val of name
  | Variant of (index * name list)
  | Call of (name * t list)
  | PatternMatching of (t * ((Pattern.var * name) list * t) list * tree)
  | Let of (name * t * t)
  | LetRec of (name * t * t)
  | Fail of (eff_name * t list)
  | Try of (t * ((eff_name * name list) * t) list)

type linkage = Public | Private

type top =
  | Value of (name * t * linkage)
  | ValueBinding of (name * name * string * linkage)
  | FunctionBinding of (name * string)
  | Exception of eff_name
  | ConstVariant of (name * index * linkage)
  | Function of (name * (name * t) * linkage)
