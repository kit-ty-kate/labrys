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

open BatteriesExceptionless
open Monomorphic.None

include Llvm

let () = enable_pretty_stacktrace ()

let build_store src dst b = ignore (build_store src dst b)
let build_ret v b = ignore (build_ret v b)
let build_ret_void b = ignore (build_ret_void b)

let define_function c s ty m =
  let f = define_function s ty m in
  f, builder_at_end c (entry_block f)

let bind c ~name s m =
  let membuffer = MemoryBuffer.of_string s in
  let m' = Llvm_irreader.parse_ir c membuffer in
  Llvm_linker.link_modules m m' Llvm_linker.Mode.DestroySource;
  Llvm.dispose_module m';
  BatOption.get (lookup_global (Gamma.Name.to_string name) m)

let optimize ~lto ~opt m =
  let pm = PassManager.create () in
  let b = Llvm_passmgr_builder.create () in
  Llvm_passmgr_builder.set_opt_level opt b;
  Llvm_passmgr_builder.populate_module_pass_manager pm b;
  if lto then begin
    Llvm_passmgr_builder.populate_lto_pass_manager
      ~internalize:true
      ~run_inliner:true
      pm
      b;
  end;
  ignore (PassManager.run_module m pm);
  Llvm.PassManager.dispose pm
