(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

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

module Name : S

module Type : S
module Constr : sig
  include S
  val to_name : t -> Name.t
  val to_type : t -> Type.t
end
module TyClass : S
module Instance : S
