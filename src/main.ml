(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open Containers
open Monomorphic.None

module Term = Cmdliner.Term
module Arg = Cmdliner.Arg

let ($) = Term.($)

let start f options modul =
  try f options modul; None with
  | Err.Exn x -> Some (Err.dump x)
  | ParserHandler.ParseError x -> Some x
  | Sys_error x -> Some x
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
  start Compiler.print_desugared_tree options modul

let start_print_untyped_tree modul src_dir build_dir lib_dir no_prelude =
  let options = object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
  end in
  start Compiler.print_untyped_tree options modul

let start_print_flatten_tree modul src_dir build_dir lib_dir no_prelude =
  let options = object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
  end in
  start Compiler.print_flatten_tree options modul

let start_print_lambda_tree modul src_dir build_dir lib_dir no_prelude =
  let options = object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
  end in
  start Compiler.print_lambda_tree options modul

let start_print_optimized_tree modul src_dir build_dir lib_dir no_prelude =
  let options = object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
  end in
  start Compiler.print_optimized_tree options modul

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
  let args = args $ Arg.(value & opt dir Config.lib & info ["lib-dir"]) in
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

let print_untyped_tree =
  let args = Term.pure start_print_untyped_tree in
  let args = base args in
  (args, Term.info "print-untyped-tree")

let print_lambda_tree =
  let args = Term.pure start_print_lambda_tree in
  let args = base args in
  (args, Term.info "print-lambda-tree")

let print_flatten_tree =
  let args = Term.pure start_print_flatten_tree in
  let args = base args in
  (args, Term.info "print-flatten-tree")

let print_optimized_tree =
  let args = Term.pure start_print_optimized_tree in
  let args = base args in
  (args, Term.info "print-optimized-tree")

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
  ; print_untyped_tree
  ; print_lambda_tree
  ; print_flatten_tree
  ; print_optimized_tree
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
