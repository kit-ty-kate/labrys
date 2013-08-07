open Llvm

val dump_value : llvalue -> unit
val dump_module : llmodule -> unit

val define_global : string -> llvalue -> llmodule -> llvalue
val define_function : string -> lltype -> llmodule -> llvalue
val builder_at_end : llcontext -> llbasicblock -> llbuilder
val entry_block : llvalue -> llbasicblock

val build_ret : llvalue -> llbuilder -> llvalue
val build_load : llvalue -> string -> llbuilder -> llvalue
val build_store : llvalue -> llvalue -> llbuilder -> llvalue
val build_extractvalue : llvalue -> int -> string -> llbuilder -> llvalue
val build_call : llvalue -> llvalue array -> string -> llbuilder -> llvalue
val build_gep : llvalue -> llvalue array -> string -> llbuilder -> llvalue

val const_struct : llcontext -> llvalue array -> llvalue
val const_null : lltype -> llvalue
val undef : lltype -> llvalue

val param : llvalue -> int -> llvalue

val create_context : unit -> llcontext
val create_module : llcontext -> string -> llmodule

val function_type : lltype -> lltype array -> lltype
val void_type : llcontext -> lltype
val i32_type : llcontext -> lltype
val i8_type : llcontext -> lltype
val struct_type : llcontext -> lltype array -> lltype
val pointer_type : lltype -> lltype
