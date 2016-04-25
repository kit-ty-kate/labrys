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

open Containers
open Monomorphic.None

let unknown_loc =
  let pos = Location.{pos_lnum = -1; pos_cnum = -1} in
  Location.{loc_start = pos; loc_end = pos; filename = "unknown"}

let prelude options =
  Module.library_create options ["Prelude"]

let prelude_name = (unknown_loc, `UpperName ["Prelude"])
let t_unit_name = `UpperName ["Prelude"; "Unit"]

let unit options =
  Ident.Type.create ~loc:unknown_loc (prelude options) "Unit"
let int options =
  Ident.Type.create ~loc:unknown_loc (prelude options) "Int"
let float options =
  Ident.Type.create ~loc:unknown_loc (prelude options) "Float"
let char options =
  Ident.Type.create ~loc:unknown_loc (prelude options) "Char"
let string options =
  Ident.Type.create ~loc:unknown_loc (prelude options) "String"

let underscore_loc loc =
  Ident.Name.local_create ~loc "_"
let underscore_type_var_loc loc =
  Ident.TypeVar.local_create ~loc "_"
let underscore_instance_loc loc =
  Ident.Instance.local_create ~loc "_"
let underscore =
  underscore_loc unknown_loc

let exn options = Ident.Type.create ~loc:unknown_loc (prelude options) "Exn"
let io options = Ident.Type.create ~loc:unknown_loc (prelude options) "IO"

let main ~current_module =
  Ident.Name.create ~loc:unknown_loc current_module "main"

let imports ~current_module options imports =
  let no_prelude = options#no_prelude in
  if no_prelude || Module.equal current_module (prelude options) then
    imports
  else
    ParseTree.Library prelude_name :: imports

let interface ~current_module options mimports tree =
  let no_prelude = options#no_prelude in
  let mimports = imports ~current_module options mimports in
  if no_prelude || Module.equal current_module (prelude options) then
    (mimports, tree)
  else
    (mimports, ParseTree.IOpen (ParseTree.Library prelude_name) :: tree)

let tree ~current_module options mimports tree =
  let no_prelude = options#no_prelude in
  let mimports = imports ~current_module options mimports in
  if no_prelude || Module.equal current_module (prelude options) then
    (mimports, tree)
  else
    (mimports, ParseTree.Open (ParseTree.Library prelude_name) :: tree)
