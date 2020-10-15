(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

module type S = sig
  include Map.S

  val find : key -> 'a t -> 'a
  val diff : 'a t -> 'a t -> 'a t
end

module Value : S with type key = Ident.Name.t
module Constr : S with type key = Ident.Constr.t
module Type : S with type key = Ident.Type.t
