(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

module Term = Cmdliner.Term
module Arg = Cmdliner.Arg

let ($) = Term.($)

let program modul src_dir build_dir lib_dir no_prelude debug lto opt o cc linkflags () =
  Compiler.compile_program modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = lib_dir
    method no_prelude = no_prelude
    method debug = debug
    method lto = lto
    method opt = opt
    method o = o
    method cc = cc
    method linkflags = linkflags
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

let print_pretyped_tree modul src_dir build_dir lib_dir no_prelude () =
  Compiler.print_pretyped_tree modul (object
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
  let doc_srcdir = "Will look for local modules to compile in DIR." in
  let doc_builddir = "Will put intermediate compiled modules in DIR." in
  let args = args $ Arg.(required & pos 0 (some string) None & info ~docv:"ModuleName" []) in
  let args = args $ Arg.(value & opt dir "" & info ~docv:"DIR" ~doc:doc_srcdir["src-dir"]) in
  let args = args $ Arg.(value & opt dir "dest" & info ~docv:"DIR" ~doc:doc_builddir ["build-dir"]) in
  args

let base args =
  let doc_libdir = "Imported library modules will be looked for in DIR." in
  let doc_noprelude =
    "Do not automatically import or open the Prelude module \
     from the standard library."
  in
  let args = restrained_base args in
  let args = args $ Arg.(value & opt dir Config.lib & info ~docv:"DIR" ~doc:doc_libdir ["lib-dir"]) in
  let args = args $ Arg.(value & flag & info ~doc:doc_noprelude ["no-prelude"]) in
  args

let base_llvm args =
  let doc_debug =
    "Add debugging information. Some information might \
     disable some optimizations."
  in
  let args = base args in
  let args = args $ Arg.(value & flag & info ~doc:doc_debug ["debug"]) in
  args

let optimization args =
  let doc_lto = "Enable the LLVM Link Time Optimizations." in
  let doc_opt = "Enable the LLVM optimizations." in
  let args = base_llvm args in
  let args = args $ Arg.(value & flag & info ~doc:doc_lto ["lto"]) in
  let args = args $ Arg.(value & opt int 0 & info ~doc:doc_opt ["opt"]) in
  args

let output args =
  let doc_output = "Write output program to FILE." in
  let doc_cc = "Use CCOMP as C linker to link your output program." in
  let doc_linkflag = "Adds a custom argument to the C linker." in
  let args = optimization args in
  let args = args $ Arg.(value & opt file "a.out" & info ~docv:"FILE" ~doc:doc_output ["o"]) in
  let args = args $ Arg.(value & opt string "cc" & info ~docv:"CCOMP" ~doc:doc_cc ["cc"]) in
  let args = args $ Arg.(value & opt_all string [] & info ~docv:"FLAG" ~doc:doc_linkflag ["linkflag"]) in
  args

let program =
  let doc = "Creates an executable." in
  let args = Term.pure program |> output in
  (args, Term.info ~doc "build-program")

let library =
  let doc = "Builds a single module and its dependencies." in
  let args = Term.pure modul |> base_llvm in
  (args, Term.info ~doc "build-module")

let print_parse_tree =
  let doc = "Prints parse-tree. (output unspecified)" in
  let args = Term.pure print_parse_tree |> restrained_base in
  (args, Term.info ~doc "print-parse-tree")

let print_desugared_tree =
  let doc = "Prints desugared-tree. (output unspecified)" in
  let args = Term.pure print_desugared_tree |> base in
  (args, Term.info ~doc "print-desugared-tree")

let print_pretyped_tree =
  let doc = "Prints pretyped-tree. (output unspecified)" in
  let args = Term.pure print_pretyped_tree |> base in
  (args, Term.info ~doc "print-pretyped-tree")

let print_untyped_tree =
  let doc = "Prints untyped-tree. (output unspecified)" in
  let args = Term.pure print_untyped_tree |> base in
  (args, Term.info ~doc "print-untyped-tree")

let print_lambda_tree =
  let doc = "Prints lambda-tree. (output unspecified)" in
  let args = Term.pure print_lambda_tree |> base in
  (args, Term.info ~doc "print-lambda-tree")

let print_flatten_tree =
  let doc = "Prints flatten-tree. (output unspecified)" in
  let args = Term.pure print_flatten_tree |> base in
  (args, Term.info ~doc "print-flatten-tree")

let print_optimized_tree =
  let doc = "Prints optimized-tree. (output unspecified)" in
  let args = Term.pure print_optimized_tree |> base in
  (args, Term.info ~doc "print-optimized-tree")

let print_early_llvm =
  let doc = "Prints LLVM-IR as generated by the backend before any optimizations." in
  let args = Term.pure print_early_llvm |> base_llvm in
  (args, Term.info ~doc "print-early-llvm")

let print_llvm =
  let doc = "Prints LLVM-IR after optimizations." in
  let args = Term.pure print_llvm |> optimization in
  (args, Term.info ~doc "print-llvm")

let default_cmd =
  let doc = "A toy language based on LLVM that implements the System F(w) type-system." in
  (Term.pure Fun.id, Term.info ~doc ~version:Config.version "labrys")

let cmds =
  [ program
  ; library
  ; print_parse_tree
  ; print_desugared_tree
  ; print_pretyped_tree
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
