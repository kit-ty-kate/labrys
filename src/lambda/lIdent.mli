(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type t

val create : string -> t

val equal : t -> t -> bool
val compare : t -> t -> int

val to_string : t -> string

module Map : Utils.EQMAP with type key = t
module Set : Utils.EQSET with type elt = t
module MSet : CCMultiSet.S with type elt = t
