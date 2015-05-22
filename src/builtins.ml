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

open BatteriesExceptionless
open Monomorphic.None

let unknown_loc =
  let pos = Location.{pos_lnum = -1; pos_cnum = -1} in
  Location.{loc_start = pos; loc_end = pos; filename = "unknown"}

let prelude options = Module.library_create options ["Prelude"]

let unit options =
  Ident.Name.create ~loc:unknown_loc (Some (prelude options)) "Unit"
let t_unit options =
  Ident.Type.create ~loc:unknown_loc (Some (prelude options)) "Unit"

let t_unit_name = `UpperName ["Prelude"; "Unit"]

let underscore_loc loc = Ident.Name.create ~loc None "_"
let underscore = underscore_loc unknown_loc

let exn = Ident.Eff.create ~loc:unknown_loc None "Exn"
let io = Ident.Eff.create ~loc:unknown_loc None "IO"

let effects = [io; exn]

let main = Ident.Name.create ~loc:unknown_loc None "main"
