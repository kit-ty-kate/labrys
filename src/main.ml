(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open Containers
open Monomorphic.None

module Term = Cmdliner.Term
module Arg = Cmdliner.Arg

let ($) = Term.($)

let program modul src_dir build_dir lib_dir no_prelude debug lto opt o () =
  Compiler.compile_program modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
    method debug = debug
    method lto = lto
    method opt = opt
    method o = o
  end)

let modul modul src_dir build_dir lib_dir no_prelude debug () =
  Compiler.compile_module modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
    method debug = debug
  end)

let print_parse_tree modul src_dir build_dir () =
  Compiler.print_parse_tree modul (object
    method src_dir = src_dir
    method build_dir = build_dir
  end)

let print_desugared_tree modul src_dir build_dir lib_dir no_prelude () =
  Compiler.print_desugared_tree modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
  end)

let print_untyped_tree modul src_dir build_dir lib_dir no_prelude () =
  Compiler.print_untyped_tree modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
  end)

let print_flatten_tree modul src_dir build_dir lib_dir no_prelude () =
  Compiler.print_flatten_tree modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
  end)

let print_lambda_tree modul src_dir build_dir lib_dir no_prelude () =
  Compiler.print_lambda_tree modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
  end)

let print_optimized_tree modul src_dir build_dir lib_dir no_prelude () =
  Compiler.print_optimized_tree modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
  end)

let print_early_llvm modul src_dir build_dir lib_dir no_prelude debug () =
  Compiler.print_early_llvm modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
    method debug = debug
  end)

let print_llvm modul src_dir build_dir lib_dir no_prelude debug lto opt () =
  Compiler.print_llvm modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
    method debug = debug
    method lto = lto
    method opt = opt
  end)

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
  let args = base_llvm args in
  let args = args $ Arg.(value & flag & info ["lto"]) in
  let args = args $ Arg.(value & opt int 0 & info ["opt"]) in
  args

let output args =
  let args = optimization args in
  let args = args $ Arg.(value & opt file "a.out" & info ["o"]) in
  args

let program =
  let args = Term.pure program |> output in
  (args, Term.info "build-program")

let library =
  let args = Term.pure modul |> base_llvm in
  (args, Term.info "build-module")

let print_parse_tree =
  let args = Term.pure print_parse_tree |> restrained_base in
  (args, Term.info "print-parse-tree")

let print_desugared_tree =
  let args = Term.pure print_desugared_tree |> base in
  (args, Term.info "print-desugared-tree")

let print_untyped_tree =
  let args = Term.pure print_untyped_tree |> base in
  (args, Term.info "print-untyped-tree")

let print_lambda_tree =
  let args = Term.pure print_lambda_tree |> base in
  (args, Term.info "print-lambda-tree")

let print_flatten_tree =
  let args = Term.pure print_flatten_tree |> base in
  (args, Term.info "print-flatten-tree")

let print_optimized_tree =
  let args = Term.pure print_optimized_tree |> base in
  (args, Term.info "print-optimized-tree")

let print_early_llvm =
  let args = Term.pure print_early_llvm |> base_llvm in
  (args, Term.info "print-early-llvm")

let print_llvm =
  let args = Term.pure print_llvm |> optimization in
  (args, Term.info "print-llvm")

let default_cmd =
  (Term.pure Fun.id, Term.info ~version:Config.version "cervoise")

let cmds =
  [ program
  ; library
  ; print_parse_tree
  ; print_desugared_tree
  ; print_untyped_tree
  ; print_lambda_tree
  ; print_flatten_tree
  ; print_optimized_tree
  ; print_early_llvm
  ; print_llvm
  ]

let () =
  match Term.eval_choice default_cmd cmds with
  | `Help -> ()
  | `Version -> ()
  | `Ok f ->
      begin try f (); exit 0 with
      | Err.Exn x -> prerr_endline (Err.dump x)
      | ParserHandler.ParseError x -> prerr_endline x
      | Sys_error x -> prerr_endline x
      | Module.Error x -> prerr_endline x
      end;
      exit 1
  | `Error _ -> exit 1
