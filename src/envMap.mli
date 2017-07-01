(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

module type S = sig
  include Utils.EQMAP

  val diff : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> string list
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
end

module Value : sig
  include S with type key = Ident.Name.t
end

module Variant : sig
  include S with type key = Ident.Variant.t
end

module Types : sig
  include S with type key = Ident.Type.t
end

module TypeVar : sig
  include S with type key = Ident.TypeVar.t
end

module Index : sig
  include S with type key = Ident.Variant.t

  val find : head_ty:Ident.Type.t -> key -> 'a t -> 'a
end

module Constr : sig
  include S with type key = Ident.Type.t

  val add : key -> Index.key -> 'a -> 'b -> ('a * 'b Index.t) t -> ('a * 'b Index.t) t

  val find : key -> 'a t -> 'a option
end

module Exn : sig
  include S with type key = Ident.Exn.t
end

module TyClass : sig
  include S with type key = Ident.TyClass.t

  val replace : key -> 'a -> 'a t -> 'a t
end

module Instance : sig
  include S with type key = Ident.Instance.t
end

module TyClassParams : sig
  include S with type key = Ident.Type.t

  val find : key -> 'a t -> 'a option
end
