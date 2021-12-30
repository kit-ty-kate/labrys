(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

open Llvm

val build_store : llvalue -> llvalue -> llbuilder -> unit
val build_ret : llvalue -> llbuilder -> unit
val build_ret_void : llbuilder -> unit
val build_br : llbasicblock -> llbuilder -> unit
val build_cond_br : llvalue -> llbasicblock -> llbasicblock -> llbuilder -> unit
val build_unreachable : llbuilder -> unit
val build_call_void : llvalue -> llvalue array -> llbuilder -> unit

val current_function : llbuilder -> llvalue
val current_param : llbuilder -> int -> llvalue

val create_block : llcontext -> llbuilder -> (llbasicblock * llbuilder)
val build_load_cast : llvalue -> lltype -> llbuilder -> llvalue

val build_ifs_unit :
  c:llcontext ->
  builder:llbuilder ->
  list:'a list ->
  ('a -> llbuilder -> llvalue) ->
  ('a -> llbuilder -> unit) ->
  (llbuilder -> unit) ->
  unit

val build_if :
  c:llcontext ->
  builder:llbuilder ->
  ty:lltype ->
  llvalue ->
  (llbuilder -> (llvalue * llbuilder)) ->
  (llbuilder -> (llvalue * llbuilder)) ->
  (llvalue * llbuilder)

val define_function :
  [`External | `Private] ->
  llcontext ->
  string ->
  lltype ->
  llmodule ->
  (llvalue * llbuilder)

val define_constant : string -> llvalue -> llmodule -> llvalue

val optimize :
  lto:bool ->
  opt:int ->
  Llvm_target.TargetMachine.t ->
  llmodule ->
  unit
