(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

let fmt = Printf.sprintf

module Name = struct
  type t = (Location.t * Module.t option * string)

  let to_string = function
    | (_, Some modul, name) -> Module.to_string modul ^ "." ^ name
    | (_, None, name) -> name

  let equal x y = String.equal (to_string x) (to_string y)
  let compare x y = String.compare (to_string x) (to_string y)

  let create ~loc modul name =
    (loc, Some modul, name)

  let local_create ~loc name =
    (loc, None, name)

  let loc (loc, _, _) = loc

  (* TODO: improve *)
  let unique (loc, modul, name) n =
    if Int.(n <= 0) then
      assert false;
    (loc, modul, fmt "%s__%d" name n)
end

module Constr = struct
  include Name
  let to_name = Fun.id
end
module Type = Name
module Exn = struct
  include Name
  let to_type = Fun.id
  let to_constr = Fun.id
end
module TyClass = Name
module Instance = Name
