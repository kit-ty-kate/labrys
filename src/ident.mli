(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

module Name : sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int

  val create : loc:Location.t -> Module.t -> string -> t
  val local_create : loc:Location.t -> string -> t
  val to_string : t -> string

  val loc : t -> Location.t

  val unique : t -> int -> t
end

module Type : module type of Name
module Constr : sig
  include module type of Name
  val to_name : t -> Name.t
  val to_type : t -> Type.t
end
module TyClass : module type of Name
module Instance : module type of Name
