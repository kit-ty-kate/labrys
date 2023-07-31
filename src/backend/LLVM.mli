(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

open Llvm

(* Low-level / Unmodified *)

module Icmp = Icmp
module Linkage = Linkage
module MemoryBuffer = MemoryBuffer

type nonrec llmodule = llmodule
type nonrec llvalue = llvalue

val create_module : llcontext -> string -> llmodule
val global_context : unit -> llcontext
val set_opaque_pointers : llcontext -> bool -> unit

val void_type : llcontext -> lltype
val i8_type : llcontext -> lltype
val i32_type : llcontext -> lltype
val float_type : llcontext -> lltype
val pointer_type : lltype -> lltype
val array_type : lltype -> int -> lltype
val function_type : lltype -> lltype array -> lltype
val var_arg_function_type : lltype -> lltype array -> lltype

val const_int : lltype -> int -> llvalue
val const_float : lltype -> float -> llvalue
val const_null : lltype -> llvalue
val const_string : llcontext -> string -> llvalue
val const_array : lltype -> llvalue array -> llvalue
val undef : lltype -> llvalue

val declare_function : string -> lltype -> llmodule -> llvalue
val declare_global : lltype -> string -> llmodule -> llvalue
val define_global : string -> llvalue -> llmodule -> llvalue

val param : llvalue -> int -> llvalue

val build_alloca : lltype -> string -> llbuilder -> llvalue
val build_call : llvalue -> llvalue array -> string -> llbuilder -> llvalue
val build_insertvalue : llvalue -> llvalue -> int -> string -> llbuilder -> llvalue
val build_load : llvalue -> string -> llbuilder -> llvalue
val build_add : llvalue -> llvalue -> string -> llbuilder -> llvalue
val build_mul : llvalue -> llvalue -> string -> llbuilder -> llvalue
val build_icmp : Icmp.t -> llvalue -> llvalue -> string -> llbuilder -> llvalue
val build_gep : llvalue -> llvalue array -> string -> llbuilder -> llvalue
val build_bitcast : llvalue -> lltype -> string -> llbuilder -> llvalue
val build_extractvalue : llvalue -> int -> string -> llbuilder -> llvalue

val size_of : lltype -> llvalue
val set_thread_local : bool -> llvalue -> unit
val set_linkage : Linkage.t -> llvalue -> unit
val is_constant : llvalue -> bool
val set_global_constant : bool -> llvalue -> unit
val is_declaration : llvalue -> bool
val value_name : llvalue -> string
val iter_globals : (llvalue -> unit) -> llmodule -> unit
val iter_functions : (llvalue -> unit) -> llmodule -> unit
val set_target_triple : string -> llmodule -> unit
val set_data_layout : string -> llmodule -> unit

val const_trunc_or_bitcast : llvalue -> lltype -> llvalue
val const_inttoptr : llvalue -> lltype -> llvalue
val const_bitcast : llvalue -> lltype -> llvalue

val string_of_llmodule : llmodule -> string

(* ----------------------- *)
(* Higher-level / Modified *)

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
