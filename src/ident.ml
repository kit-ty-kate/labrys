(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

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
