(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

open Llvm

let () = enable_pretty_stacktrace ()

let build_store src dst b = ignore (build_store src dst b)
let build_ret v b = ignore (build_ret v b)
let build_ret_void b = ignore (build_ret_void b)
let build_br bb b = ignore (build_br bb b)
let build_cond_br cond tbb fbb b = ignore (build_cond_br cond tbb fbb b)
let build_unreachable b = ignore (build_unreachable b)
let build_call_void f params b = ignore (build_call f params "" b)

let current_function builder = block_parent (insertion_block builder)
let current_param builder = param (current_function builder)

let create_block c builder =
  let func = current_function builder in
  let block = append_block c "block" func in
  let builder = builder_at_end c block in
  (block, builder)

let build_load_cast v ty builder =
  let v = build_bitcast v ty "" builder in
  build_load v "" builder

let define_function linkage c s ty m =
  let linkage = match linkage with
    | `External -> Linkage.External
    | `Private -> Linkage.Private
  in
  let f = define_function s ty m in
  set_unnamed_addr true f;
  set_linkage linkage f;
  f, builder_at_end c (entry_block f)

let define_constant name v m =
  let v = define_global name v m in
  set_unnamed_addr true v;
  set_linkage Linkage.Private v;
  set_global_constant true v;
  v

let optimize ~lto ~opt target m =
  let pm = PassManager.create () in
  let pm_f = PassManager.create_function m in (* TODO: Is this useful ? *)
  Llvm_target.TargetMachine.add_analysis_passes pm target; (* TODO Is this useful ? *)
  let b = Llvm_passmgr_builder.create () in
  Llvm_passmgr_builder.set_opt_level opt b;
  Llvm_scalar_opts.add_tail_call_elimination pm;
  Llvm_passmgr_builder.populate_module_pass_manager pm b;
  Llvm_passmgr_builder.populate_function_pass_manager pm_f b; (* TODO Is this useful ? *)
  if lto then begin
    Llvm_passmgr_builder.populate_lto_pass_manager
      ~internalize:true
      ~run_inliner:true
      pm
      b;
  end;
  ignore (PassManager.run_module m pm);
  PassManager.dispose pm
