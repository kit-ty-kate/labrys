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

type name = Gamma.Name.t
type t_name = Gamma.Type.t

type t_value = (t_name * Kinds.t)

type ty =
  | Fun of (ty * ty)
  | Ty of t_name
  | Forall of (t_name * Kinds.t * ty)
  | AbsOnTy of (t_name * Kinds.t * ty)
  | AppOnTy of (ty * ty)

type value = (name * ty)

type pattern =
  | TyConstr of name
  | Any of name
  | PatternApp of (pattern * pattern)
  | PatternTApp of (pattern * ty)

type t =
  | Abs of (Location.t * value * t)
  | TAbs of (Location.t * t_value * t)
  | App of (Location.t * t * t)
  | TApp of (Location.t * t * ty)
  | Val of (Location.t * name)
  | PatternMatching of (Location.t * t * (pattern * t) list)

type variant =
  | Variant of (Location.t * name * ty)

type top =
  | Value of (name * t)
  | Type of (Location.t * t_name * ty)
  | Binding of (Location.t * name * ty * string)
  | Datatype of (Location.t * t_name * Kinds.t * variant list)
