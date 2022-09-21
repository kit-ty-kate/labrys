(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

exception Error of string

type t

val create : current_module:t -> string list -> t
val from_filename :
  <src_dir : string; build_dir : string; ..> ->
  string ->
  t
val from_module_name :
  <src_dir : string; build_dir : string; ..> ->
  string ->
  t

val library_create : <lib_dir : string; ..> -> string list -> t
val library_from_module_name : <lib_dir : string; ..> -> string -> t

val impl : t -> string
val cimpl : t -> string
val impl_infos : t -> string
val intf : t -> string

val to_string : t -> string

val is_library : t -> bool

val equal : t -> t -> bool
val compare : t -> t -> int

val to_module : t -> string list

module Map : Map.S with type key = t
