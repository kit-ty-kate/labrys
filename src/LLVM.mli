type lltype
type llvalue
type llmodule
type llbuilder

exception TypeMissmatch of string * lltype

val create_module : string -> llmodule

val define_global : string -> llvalue -> llmodule -> llvalue
val define_function : string -> lltype -> llmodule -> llvalue * llbuilder

val build_ret : llvalue -> llbuilder -> unit
val build_ret_void : llbuilder -> unit
val build_load : llvalue -> string -> llbuilder -> llvalue
val build_store : llvalue -> llvalue -> llbuilder -> unit
val build_extractvalue : llvalue -> int -> string -> llbuilder -> llvalue
val build_insertvalue : llvalue -> llvalue -> int -> llbuilder -> unit
val build_call : llvalue -> llvalue list -> string -> llbuilder -> llvalue
val build_gep : llvalue -> llvalue list -> string -> llbuilder -> llvalue
val build_malloc : lltype -> string -> llbuilder -> llvalue
val build_bitcast : llvalue -> lltype -> string -> llbuilder -> llvalue

val const_int : lltype -> int -> llvalue
val const_struct : llvalue list -> llvalue
val undef : lltype -> llvalue

val param : llvalue -> int -> llvalue

val function_type : lltype -> lltype list -> lltype
val void_type : lltype
val i64_type : lltype
val i32_type : lltype
val i8_type : lltype
val struct_type : lltype list -> lltype
val pointer_type : lltype -> lltype
val array_type : lltype -> int -> lltype

val to_string : llmodule -> string
