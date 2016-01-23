(*
Copyright (c) 2013-2015 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

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

open Monomorphic_containers.Open

open Llvm

let () = enable_pretty_stacktrace ()

let build_store src dst b = ignore (build_store src dst b)
let build_ret v b = ignore (build_ret v b)
let build_ret_void b = ignore (build_ret_void b)
let build_br bb b = ignore (build_br bb b)
let build_cond_br cond tbb fbb b = ignore (build_cond_br cond tbb fbb b)
let build_unreachable b = ignore (build_unreachable b)
let build_call_void f params b = ignore (build_call f params "" b)

let current_function builder = Llvm.block_parent (Llvm.insertion_block builder)
let current_param builder = Llvm.param (current_function builder)

let create_block c builder =
  let func = current_function builder in
  let block = Llvm.append_block c "block" func in
  let builder = Llvm.builder_at_end c block in
  (block, builder)

let build_load_cast v ty builder =
  let v = Llvm.build_bitcast v ty "" builder in
  Llvm.build_load v "" builder

let define_function linkage c s ty m =
  let linkage = match linkage with
    | `External -> Linkage.External
    | `Private -> Linkage.Private
  in
  let f = define_function s ty m in
(* NOTE: Uncomment this when switching to LLVM 3.8 *)
(*  Llvm.set_unnamed_addr true f; *)
  Llvm.set_linkage linkage f;
  f, builder_at_end c (entry_block f)

let define_constant name v m =
  let v = define_global name v m in
(* NOTE: Uncomment this when switching to LLVM 3.8 *)
(*  Llvm.set_unnamed_addr true v; *)
  Llvm.set_linkage Llvm.Linkage.Private v;
  Llvm.set_global_constant true v;
  v

let optimize ~lto ~opt layout m =
  let pm = PassManager.create () in
  Llvm_target.DataLayout.add_to_pass_manager pm layout;
  let b = Llvm_passmgr_builder.create () in
  Llvm_passmgr_builder.set_opt_level opt b;
  Llvm_scalar_opts.add_tail_call_elimination pm;
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
