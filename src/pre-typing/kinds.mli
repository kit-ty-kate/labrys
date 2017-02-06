(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type t =
  | Star
  | Eff
  | KFun of (t * t)

val from_list : t list -> t

val to_string : t -> string

val equal : t -> t -> bool

val not_star : t -> bool

val is_effect : t -> bool

module Err : sig
  val fail : loc:Location.t -> has:t -> expected:t -> 'a
end
