(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

let fmt = Printf.sprintf

module type S = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int

  val create : loc:Location.t -> Module.t -> string -> t
  val local_create : loc:Location.t -> string -> t
  val to_string : t -> string

  val loc : t -> Location.t

  val unique : t -> int -> t

  module Set : Set.S with type elt = t
  module MSet : CCMultiSet.S with type elt = t
end

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

  type tmp = t

  module Set = Set.Make (struct
      type t = tmp
      let compare = compare
    end)
  module MSet = CCMultiSet.Make (struct
      type t = tmp
      let compare = compare
    end)
end

module Type = Name
module Constr = struct
  include Name
  let to_name = Fun.id
  let to_type = Fun.id
end
module TyClass = Name
module Instance = Name
