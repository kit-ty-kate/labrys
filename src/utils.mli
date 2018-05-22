(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

val string_of_uchar : Uchar.t -> string

val string_of_doc : PPrint.document -> string

val string_of_list : ('a -> string) -> 'a list -> string

val mkdir : string -> unit

val remove_last : 'a list -> 'a list
val detach_last : 'a list -> ('a list * 'a)

val last : 'a list -> 'a

val swap_list : int -> 'a list -> 'a list

val combine_compare : (unit -> int) list -> int

val exec_command : string -> string list -> int

module CCIO : module type of CCIO

module PPrint : sig
  include module type of PPrint

  val str : string -> document
  val (^^^) : document -> document -> document
end
