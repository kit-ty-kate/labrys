(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

val string_of_list : ('a -> string) -> 'a list -> string

val mkdir : string -> unit

val remove_last : 'a list -> 'a list
val detach_last : 'a list -> ('a list * 'a)

val last : 'a list -> 'a

val combine_compare : (unit -> int) list -> int

val exec_command : string -> string list -> int

module StrListSet : CCSet.S with type elt = string list

module type EQ = sig
  type t

  val equal : t -> t -> bool
end

module type EQSET = sig
  type elt
  type t

  val empty : t
  val mem : elt -> t -> bool
  val fold : (elt -> 'b -> 'b) -> t -> 'b -> 'b
  val add : elt -> t -> t
  val singleton : elt -> t
  val of_list : elt list -> t
  val remove : elt -> t -> t
  val is_empty : t -> bool
  val cardinal : t -> int
  val map : (elt -> elt) -> t -> t
  val for_all : (elt -> bool) -> t -> bool
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val union : t -> t -> t
  val union3 : t -> t -> t -> t
end

module type EQMAP = sig
  type key
  type 'a t

  module Set : EQSET with type elt = key

  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a option
  val modify_def : 'a -> key -> ('a -> 'a) -> 'a t -> 'a t
  val bindings : 'a t -> (key * 'a) list
  val singleton : key -> 'a -> 'a t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
  val map_keys : (key -> key) -> 'a t -> 'a t
  val find_binding : key -> 'a t -> (key * 'a) option
  val merge : 'a t -> 'a t -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val to_set : _ t -> Set.t
end

module EqMap (I : EQ) : EQMAP with type key = I.t
module EqSet (I : EQ) : EQSET with type elt = I.t

module CCIO : module type of CCIO
