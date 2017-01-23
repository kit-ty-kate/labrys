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

let fmt = Printf.sprintf

module Name = struct
  type t = (Location.t * Module.t option * string)

  let equal (_, module_x, x) (_, module_y, y) = match module_x, module_y with
    | Some module_x, Some module_y ->
        Module.equal module_x module_y && String.equal x y
    | None, None ->
        String.equal x y
    | Some _, None | None, Some _ ->
        false

  let to_string = function
    | (_, Some modul, name) ->
        Module.to_string modul ^ "." ^ name
    | (_, None, name) ->
        name

  let compare x y = String.compare (to_string x) (to_string y)

  let create ~loc modul name =
    (loc, Some modul, name)

  let local_create ~loc name =
    (loc, None, name)

  let loc (loc, _, _) = loc

  let unique (loc, modul, name) n =
    if Int.(n <= 0) then
      assert false;
    (loc, modul, fmt "%s__%d" name n)

  let prepend_empty (loc, modul, name) =
    (loc, modul, "." ^ name)
end

module Variant = struct
  include Name

  let to_name = Fun.id
end

module Type = Name

module TypeVar = Name

module Exn = Name

module TyClass = Name

module Instance = struct
  include Name

  let to_name (loc, modul, name) =
    (loc, modul, "$named-instance$" ^ name)
end
