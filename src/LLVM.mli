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

exception BackendFailure of string

include module type of struct include Llvm end

val build_store : llvalue -> llvalue -> llbuilder -> unit
val build_ret : llvalue -> llbuilder -> unit
val build_ret_void : llbuilder -> unit
val build_br : llbasicblock -> llbuilder -> unit
val build_cond_br : llvalue -> llbasicblock -> llbasicblock -> llbuilder -> unit
val build_unreachable : llbuilder -> unit
val build_call_void : llvalue -> llvalue array -> llbuilder -> unit

val define_function : llcontext -> string -> lltype -> llmodule -> (llvalue * llbuilder)

val bind : llcontext -> name:Ident.Name.t -> arity:int -> string -> llmodule -> llvalue

val optimize :
  lto:bool ->
  opt:int ->
  Llvm_target.DataLayout.t ->
  llmodule ->
  unit
