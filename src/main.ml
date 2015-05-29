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

open Monomorphic_containers

module Term = Cmdliner.Term
module Arg = Cmdliner.Arg

let ($) = Term.($)

let start f options modul =
  try f options modul; None with
  | Err.Exn x -> Some (Err.dump x)
  | ParserHandler.ParseError x -> Some x
  | Sys_error x -> Some x
  | Llvm_irreader.Error x -> Some x
  | Module.Error x -> Some x

let start_program modul src_dir build_dir lib_dir no_prelude debug lto opt o =
  let options = object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
    method debug = debug
    method lto = lto
    method opt = opt
    method o = o
  end in
  start Compiler.compile_program options modul

let start_module modul src_dir build_dir lib_dir no_prelude debug =
  let options = object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
    method debug = debug
  end in
  start Compiler.compile_module options modul

let start_print_parse_tree modul src_dir build_dir =
  let options = object
    method src_dir = src_dir
    method build_dir = build_dir
  end in
  start Compiler.print_parse_tree options modul

let start_print_unsugared_tree modul src_dir build_dir lib_dir no_prelude =
  let options = object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
  end in
  start Compiler.print_unsugared_tree options modul

let start_print_typed_tree modul src_dir build_dir lib_dir no_prelude =
  let options = object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
  end in
  start Compiler.print_typed_tree options modul

let start_print_untyped_tree modul src_dir build_dir lib_dir no_prelude =
  let options = object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
  end in
  start Compiler.print_untyped_tree options modul

let start_print_early_llvm modul src_dir build_dir lib_dir no_prelude debug =
  let options = object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
    method debug = debug
  end in
  start Compiler.print_early_llvm options modul

let start_print_llvm modul src_dir build_dir lib_dir no_prelude debug lto opt =
  let options = object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
    method debug = debug
    method lto = lto
    method opt = opt
  end in
  start Compiler.print_llvm options modul

let restrained_base args =
  let args = args $ Arg.(required & pos 0 (some string) None & info []) in
  let args = args $ Arg.(value & opt dir "" & info ["src-dir"]) in
  let args = args $ Arg.(value & opt dir "dest" & info ["build-dir"]) in
  args

let base args =
  let args = restrained_base args in
  let args = args $ Arg.(value & opt dir "stdlib" & info ["lib-dir"]) in
  let args = args $ Arg.(value & flag & info ["no-prelude"]) in
  args

let base_llvm args =
  let args = base args in
  let args = args $ Arg.(value & flag & info ["debug"]) in
  args

let optimization args =
  let args = args $ Arg.(value & flag & info ["lto"]) in
  let args = args $ Arg.(value & opt int 0 & info ["opt"]) in
  args

let program =
  let args = Term.pure start_program in
  let args = base_llvm args in
  let args = optimization args in
  let args = args $ Arg.(value & opt file "a.out" & info ["o"]) in
  (args, Term.info "build-program")

let library =
  let args = Term.pure start_module in
  let args = base_llvm args in
  (args, Term.info "build-module")

let print_parse_tree =
  let args = Term.pure start_print_parse_tree in
  let args = restrained_base args in
  (args, Term.info "print-parse-tree")

let print_unsugared_tree =
  let args = Term.pure start_print_unsugared_tree in
  let args = base args in
  (args, Term.info "print-unsugared-tree")

let print_typed_tree =
  let args = Term.pure start_print_typed_tree in
  let args = base args in
  (args, Term.info "print-typed-tree")

let print_untyped_tree =
  let args = Term.pure start_print_untyped_tree in
  let args = base args in
  (args, Term.info "print-untyped-tree")

let print_early_llvm =
  let args = Term.pure start_print_early_llvm in
  let args = base_llvm args in
  (args, Term.info "print-early-llvm")

let print_llvm =
  let args = Term.pure start_print_llvm in
  let args = base_llvm args in
  let args = optimization args in
  (args, Term.info "print-llvm")

let default_cmd =
  (Term.pure None, Term.info ~version:Config.version "cervoise")

let cmds =
  [ program
  ; library
  ; print_parse_tree
  ; print_unsugared_tree
  ; print_typed_tree
  ; print_untyped_tree
  ; print_early_llvm
  ; print_llvm
  ]

let () =
  match Term.eval_choice default_cmd cmds with
  | `Help
  | `Version
  | `Ok None -> exit 0
  | `Ok (Some x) -> prerr_endline x; exit 1
  | `Error _ -> exit 1
