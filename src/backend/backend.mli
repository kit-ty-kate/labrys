(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type t

val make :
  modul:Module.t ->
  imports:Module.t list ->
  <debug : bool; ..> ->
  OptimizedTree.top list ->
  t

val link : main_module_name:Module.t -> main_module:t -> t Module.Map.t -> t

val optimize : <lto : bool; opt : int; ..> -> t -> t

val to_string : t -> string

val write_bitcode : o:string -> t -> unit
val read_bitcode : string -> t

val emit_object_file : tmp:string -> t -> unit
