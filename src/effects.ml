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

let fmt = Printf.sprintf

module Exn_set = Set.Make(Ident.Exn)

type t =
  { exn : Exn_set.t
  }

let empty =
  { exn = Exn_set.empty
  }

let is_empty {exn} =
  Exn_set.is_empty exn

let equal x y =
  Exn_set.equal x.exn y.exn

let add_exn exn self =
  let exn = Exn_set.add exn self.exn in
  {self with exn}

let union x y =
  let exn = Exn_set.union x.exn y.exn in
  {exn}

let union3 x y z =
  let exn = Exn_set.union x.exn y.exn in
  let exn = Exn_set.union exn z.exn in
  {exn}

let remove_exn x self =
  let exn = Exn_set.remove x self.exn in
  {self with exn}

let to_string {exn} =
  let exn = List.map Ident.Exn.to_string (Exn_set.to_list exn) in
  let exn = fmt "Exn [%s]" (String.concat " | " exn) in
  fmt "[%s]" exn

let to_string x =
  if is_empty x then
    ""
  else
    to_string x
