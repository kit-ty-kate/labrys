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

let fmt = Printf.sprintf

type 'a module_opt =
  | ModNone
  | ModFake
  | ModSome of 'a

module Name = struct
  type t = (Location.t * Module.t module_opt * string)

  let compare (_, module_x, x) (_, module_y, y) =
    let cmp_module = match module_x, module_y with
      | ModNone, (ModSome _ | ModFake) -> -1
      | ModFake, (ModSome _ | ModNone) -> 1
      | ModSome _, (ModNone | ModFake) -> 1
      | ModNone, ModNone -> 0
      | ModFake, ModFake -> 0
      | ModSome module_x, ModSome module_y -> Module.compare module_x module_y
    in
    if Int.(cmp_module = 0) then
      String.compare x y
    else
      cmp_module

  let equal x y = Int.equal (compare x y) 0

  let prepend modul = function
    | (loc, ModNone, name) -> (loc, ModSome modul, name)
    | (_, ModFake, _) -> assert false
    | (_, ModSome _, _) as x -> x

  let prepend_empty = function
    | (loc, ModNone, name) -> (loc, ModFake, name)
    | (_, ModFake, _) -> assert false
    | (_, ModSome _, _) as x -> x

  let create ~loc modul name =
    let modul = match modul with
      | None -> ModNone
      | Some modul -> ModSome modul
    in
    (loc, modul, name)

  let to_string = function
    | (_, ModSome modul, name) -> Module.to_string modul ^ "." ^ name
    | (_, ModFake, name) -> "." ^ name
    | (_, ModNone, name) -> name

  let loc (loc, _, _) = loc

  let unique (loc, modul, name) n = match modul with
    | ModNone -> (* TODO: Improve here *)
        (loc, ModNone, fmt "%s__%d" name n)
    | ModFake
    | ModSome _ ->
        assert false
end

module Type = Name

module Exn = Name

module Eff = Name
