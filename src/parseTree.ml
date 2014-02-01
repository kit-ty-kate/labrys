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

open BatteriesExceptionless
open Monomorphic.None

type ty =
  | Fun of (ty * ty)
  | Ty of string
  | Forall of (string * ty)

type value = (string * ty)

type kind =
  | Star
  | KFun of (kind * kind)

type t_value = (string * kind option)

type position = {pos_lnum : int; pos_cnum : int}
type location = {loc_start : position; loc_end : position}

type t =
  | Abs of (location * value * t)
  | TAbs of (location * t_value * t)
  | App of (location * t * t)
  | TApp of (location * t * ty)
  | Val of (location * string)

type variant =
  | Variant of (location * string * ty)

type top =
  | Value of (location * string * t)
  | Type of (location * string * ty)
  | Binding of (location * string * ty * string)
  | Datatype of (location * string * variant list)
