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

let fmt = Printf.sprintf

exception BackendFailure of string

include Llvm

let () = enable_pretty_stacktrace ()

let build_store src dst b = ignore (build_store src dst b)
let build_ret v b = ignore (build_ret v b)
let build_ret_void b = ignore (build_ret_void b)
let build_br bb b = ignore (build_br bb b)
let build_cond_br cond tbb fbb b = ignore (build_cond_br cond tbb fbb b)
let build_unreachable b = ignore (build_unreachable b)
let build_call_void f params b = ignore (build_call f params "" b)

let define_function c s ty m =
  let f = define_function s ty m in
  f, builder_at_end c (entry_block f)

let bind c ~name ~arity s m =
  let membuffer = MemoryBuffer.of_string s in
  let m' = Llvm_irreader.parse_ir c membuffer in
  let name = Ident.Name.to_string name in
  let set_link_priv v =
    if not (is_declaration v || String.equal (value_name v) name) then
      set_linkage Linkage.Private v
  in
  iter_globals set_link_priv m';
  iter_functions set_link_priv m';
  Llvm_linker.link_modules m m' Llvm_linker.Mode.DestroySource;
  dispose_module m';
  let lookup = if Int.(arity = 0) then lookup_global else lookup_function in
  match lookup name m with
  | Some v ->
      set_linkage Linkage.Private v;
      if Int.(arity > 0) then begin
        let len = Array.length (params v) in
        if Int.(len <> arity) then
          raise (BackendFailure (fmt "Arity doesn't match for the LLVM binding '%s'. Expected %d, got %d" name arity len));
      end;
      v
  | None ->
      raise (BackendFailure (fmt "Cannot found the LLVM binding '%s'" name))

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
