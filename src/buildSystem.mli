(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

exception Failure

val check_impl :
  <src_dir: string; build_dir : string; lib_dir : string; ..> ->
  Module.t ->
  Module.t list
val write_impl_infos : Module.t list -> Module.t -> unit
