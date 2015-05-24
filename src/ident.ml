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

module Name = struct
  type t = (Location.t * Module.t option * string)

  let compare (_, module_x, x) (_, module_y, y) =
    let module_x = Option.default_delayed (fun () -> assert false) module_x in
    let module_y = Option.default_delayed (fun () -> assert false) module_y in
    let cmp_module = Module.compare module_x module_y in
    if Int.equal cmp_module 0 then
      String.compare x y
    else
      cmp_module

  let equal x y = Int.equal (compare x y) 0

  let fill_module ~matches:(_, modul, matches_name) = function
    | ((loc, None, name) as self) ->
        if String.equal matches_name name then
          let modul = Option.default_delayed (fun () -> assert false) modul in
          ((loc, Some modul, name), true)
        else
          (self, false)
    | ((_, Some _, _) as self) ->
        (self, true)

  let create ~loc modul name =
    (loc, Some modul, name)

  let local_create ~loc name =
    (loc, None, name)

  let to_string = function
    | (_, Some modul, name) ->
        Module.to_string modul ^ "." ^ name
    | (_, None, name) ->
        name

  let loc (loc, _, _) = loc

  let unique (loc, modul, name) n =
    let modul = Option.default_delayed (fun () -> assert false) modul in
    (loc, Some modul, fmt "%s__%d" name n)

  let remove_aliases = function
    | (loc, Some modul, name) ->
        (loc, Some (Module.remove_aliases modul), name)
    | (_, None, _) as self ->
        self

  let open_module m = function
    | (loc, Some modul, name) as self ->
        if Module.equal m modul then
          (loc, Some (Module.open_module modul), name)
        else
          self
    | (_, None, _) as self ->
        self

  let prepend_empty = function
    | (loc, Some modul, name) ->
        (loc, Some modul, "." ^ name)
    | (_, None, _) ->
        assert false
end

module Type = Name

module Exn = Name

module Eff = struct
  type t = (Location.t * string)

  let compare (_, x) (_, y) =
    String.compare x y
  let equal (_, x) (_, y) = String.equal x y

  let create ~loc name = (loc, name)
  let to_string (_, name) = name

  let loc (loc, _) = loc
end
