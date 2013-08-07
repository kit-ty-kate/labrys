let check_null t msg = failwith ("This function expected of type " ^ msg ^ " but the parameter is of type " ^ Llvm.string_of_lltype t)
let check_null' v  = check_null (Llvm.type_of v)

let dump_value = Llvm.dump_value
let dump_module = Llvm.dump_module

let define_global = Llvm.define_global
let define_function = Llvm.define_function
let builder_at_end = Llvm.builder_at_end
let entry_block = Llvm.entry_block

let build_ret = Llvm.build_ret
let build_load = Llvm.build_load
let build_store = Llvm.build_store
let build_extractvalue= Llvm.build_extractvalue
let build_call = Llvm.build_call
let build_gep = Llvm.build_gep

let const_struct = Llvm.const_struct
let const_null = Llvm.const_null
let undef = Llvm.undef
let void_type = Llvm.void_type

let param = Llvm.param

let create_context = Llvm.create_context
let create_module = Llvm.create_module

let function_type = Llvm.function_type
let i32_type = Llvm.i32_type
let i8_type = Llvm.i8_type
let struct_type = Llvm.struct_type
let pointer_type = Llvm.pointer_type
