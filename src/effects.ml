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
  ; io : bool
  }

let empty =
  { exn = Exn_set.empty
  ; io = false
  }

let is_empty {exn; io} =
  Exn_set.is_empty exn
  && not io

let equal x y =
  Exn_set.equal x.exn y.exn
  && Bool.equal x.io y.io

let is_subset_of x y =
  Exn_set.subset x.exn y.exn
  && ((x.io && y.io) || not x.io)

let has_io x = x.io

let add ~loc (name, args) self =
  match Ident.Eff.to_string name with
  | "IO" ->
      {self with io = true}
  | "Exn" ->
      let args = Exn_set.of_list args in
      let exn = Exn_set.union args self.exn in
      {self with exn}
  | name ->
      Error.fail ~loc "Unknown effect '%s'" name

let add_exn x self =
  let exn = Exn_set.add x self.exn in
  {self with exn}

let union x y =
  let exn = Exn_set.union x.exn y.exn in
  let io = x.io || y.io in
  {exn; io}

let union3 x y z =
  let exn = Exn_set.union x.exn y.exn in
  let exn = Exn_set.union exn z.exn in
  let io = x.io || y.io || z.io in
  {exn; io}

let remove_exn x self =
  let exn = Exn_set.remove x self.exn in
  {self with exn}

let to_string {exn; io} =
  let exn = List.map Ident.Exn.to_string (Exn_set.to_list exn) in
  let exn = fmt "Exn [%s]" (String.concat " | " exn) in
  let io = if io then "IO, " else "" in
  fmt "-[%s%s]->" io exn

let to_string x =
  if Exn_set.is_empty x.exn then
    if x.io then
      "-[IO]->"
    else
      "->"
  else
    to_string x
