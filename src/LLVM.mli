(*
Copyright (c) 2013 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

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

val bind : name:string -> ty:lltype -> string -> llmodule -> llvalue

val to_string : llmodule -> string
