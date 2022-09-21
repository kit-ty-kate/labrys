(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

module Term = Cmdliner.Term
module Arg = Cmdliner.Arg
module Cmd = Cmdliner.Cmd

let ($) = Term.($)

let program modul build_dir lib_dir no_prelude debug initial_heap_size lto opt o cc linkflags () =
  let src_dir = Filename.dirname modul in
  let modul = Filename.basename modul in
  Compiler.compile_program modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = Lazy.force lib_dir
    method no_prelude = no_prelude
    method debug = debug
    method initial_heap_size = initial_heap_size
    method lto = lto
    method opt = opt
    method o = o
    method cc = cc
    method linkflags = linkflags
  end)

let modul modul build_dir lib_dir no_prelude debug initial_heap_size () =
  let src_dir = Filename.dirname modul in
  let modul = Filename.basename modul in
  Compiler.compile_module modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = Lazy.force lib_dir
    method no_prelude = no_prelude
    method debug = debug
    method initial_heap_size = initial_heap_size
  end)

let print_parse_tree modul build_dir () =
  let src_dir = Filename.dirname modul in
  let modul = Filename.basename modul in
  Compiler.print_parse_tree modul (object
    method src_dir = src_dir
    method build_dir = build_dir
  end)

let print_desugared_tree modul build_dir lib_dir no_prelude () =
  let src_dir = Filename.dirname modul in
  let modul = Filename.basename modul in
  Compiler.print_desugared_tree modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = Lazy.force lib_dir
    method no_prelude = no_prelude
  end)

let print_pretyped_tree modul build_dir lib_dir no_prelude () =
  let src_dir = Filename.dirname modul in
  let modul = Filename.basename modul in
  Compiler.print_pretyped_tree modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = Lazy.force lib_dir
    method no_prelude = no_prelude
  end)

let print_untyped_tree modul build_dir lib_dir no_prelude () =
  let src_dir = Filename.dirname modul in
  let modul = Filename.basename modul in
  Compiler.print_untyped_tree modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = Lazy.force lib_dir
    method no_prelude = no_prelude
  end)

let print_flatten_tree modul build_dir lib_dir no_prelude () =
  let src_dir = Filename.dirname modul in
  let modul = Filename.basename modul in
  Compiler.print_flatten_tree modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = Lazy.force lib_dir
    method no_prelude = no_prelude
  end)

let print_lambda_tree modul build_dir lib_dir no_prelude () =
  let src_dir = Filename.dirname modul in
  let modul = Filename.basename modul in
  Compiler.print_lambda_tree modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = Lazy.force lib_dir
    method no_prelude = no_prelude
  end)

let print_optimized_tree modul build_dir lib_dir no_prelude () =
  let src_dir = Filename.dirname modul in
  let modul = Filename.basename modul in
  Compiler.print_optimized_tree modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = Lazy.force lib_dir
    method no_prelude = no_prelude
  end)

let print_early_llvm modul build_dir lib_dir no_prelude debug initial_heap_size () =
  let src_dir = Filename.dirname modul in
  let modul = Filename.basename modul in
  Compiler.print_early_llvm modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = Lazy.force lib_dir
    method no_prelude = no_prelude
    method debug = debug
    method initial_heap_size = initial_heap_size
  end)

let print_llvm modul build_dir lib_dir no_prelude debug initial_heap_size lto opt () =
  let src_dir = Filename.dirname modul in
  let modul = Filename.basename modul in
  Compiler.print_llvm modul (object
    method src_dir = src_dir
    method build_dir = build_dir
    method lib_dir = Lazy.force lib_dir
    method no_prelude = no_prelude
    method debug = debug
    method initial_heap_size = initial_heap_size
    method lto = lto
    method opt = opt
  end)

let restrained_base args =
  let doc_builddir = "Will put intermediate compiled modules in DIR." in
  let args = args $ Arg.(required & pos 0 (some string) None & info ~docv:"FILE" []) in
  let args = args $ Arg.(value & opt dir "dest" & info ~docv:"DIR" ~doc:doc_builddir ["build-dir"]) in
  args

let lazy_dir =
  let parse s = Result.map Lazy.from_val (Config.parse_dir s) in
  let print fmt dir = Format.pp_print_string fmt (Lazy.force dir) in
  Arg.conv (parse, print)

let base args =
  let doc_libdir = "Imported library modules will be looked for in DIR." in
  let doc_noprelude =
    "Do not automatically import or open the Prelude module \
     from the standard library."
  in
  let args = restrained_base args in
  let args = args $ Arg.(value & opt lazy_dir Config.lib & info ~docv:"DIR" ~doc:doc_libdir ["lib-dir"]) in
  let args = args $ Arg.(value & flag & info ~doc:doc_noprelude ["no-prelude"]) in
  args

let base_llvm args =
  let doc_debug =
    "Add debugging information. Some information might \
     disable some optimizations."
  in
  let doc_initial_heap_size = "Defines the initial heap sized allocated on startup. \
                               Default: "^string_of_int Backend.default_heap_size in
  let args = base args in
  let args = args $ Arg.(value & flag & info ~doc:doc_debug ["debug"]) in
  let args = args $ Arg.(value & opt int Backend.default_heap_size & info ~docv:"SIZE" ~doc:doc_initial_heap_size ["initial-heap-size"]) in
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
  let args = Term.const program |> output in
  Cmd.v (Cmd.info ~doc "build-program") args

let library =
  let doc = "Builds a single module and its dependencies." in
  let args = Term.const modul |> base_llvm in
  Cmd.v (Cmd.info ~doc "build-module") args

let print_parse_tree =
  let doc = "Prints parse-tree. (output unspecified)" in
  let args = Term.const print_parse_tree |> restrained_base in
  Cmd.v (Cmd.info ~doc "print-parse-tree") args

let print_desugared_tree =
  let doc = "Prints desugared-tree. (output unspecified)" in
  let args = Term.const print_desugared_tree |> base in
  Cmd.v (Cmd.info ~doc "print-desugared-tree") args

let print_pretyped_tree =
  let doc = "Prints pretyped-tree. (output unspecified)" in
  let args = Term.const print_pretyped_tree |> base in
  Cmd.v (Cmd.info ~doc "print-pretyped-tree") args

let print_untyped_tree =
  let doc = "Prints untyped-tree. (output unspecified)" in
  let args = Term.const print_untyped_tree |> base in
  Cmd.v (Cmd.info ~doc "print-untyped-tree") args

let print_lambda_tree =
  let doc = "Prints lambda-tree. (output unspecified)" in
  let args = Term.const print_lambda_tree |> base in
  Cmd.v (Cmd.info ~doc "print-lambda-tree") args

let print_flatten_tree =
  let doc = "Prints flatten-tree. (output unspecified)" in
  let args = Term.const print_flatten_tree |> base in
  Cmd.v (Cmd.info ~doc "print-flatten-tree") args

let print_optimized_tree =
  let doc = "Prints optimized-tree. (output unspecified)" in
  let args = Term.const print_optimized_tree |> base in
  Cmd.v (Cmd.info ~doc "print-optimized-tree") args

let print_early_llvm =
  let doc = "Prints LLVM-IR as generated by the backend before any optimizations." in
  let args = Term.const print_early_llvm |> base_llvm in
  Cmd.v (Cmd.info ~doc "print-early-llvm") args

let print_llvm =
  let doc = "Prints LLVM-IR after optimizations." in
  let args = Term.const print_llvm |> optimization in
  Cmd.v (Cmd.info ~doc "print-llvm") args

let default =
  let aux git_version () =
    if git_version then
      print_endline Config.git_version;
  in
  let doc_git_version = "Print the git version of labrys, if set, and exit." in
  let args = Term.const aux in
  let args = args $ Arg.(value & flag & info ~doc:doc_git_version ["git-version"]) in
  args

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
  let doc = "A toy language based on LLVM that implements the System F(w) type-system." in
  let info = Cmd.info ~doc ~version:Config.version "labrys" in
  match Cmd.eval_value (Cmd.group ~default info cmds) with
  | Ok `Help -> ()
  | Ok `Version -> ()
  | Ok (`Ok f) ->
      begin try f (); exit 0 with
      | Err.Exn x -> prerr_endline (Err.dump x)
      | ParserHandler.ParseError x -> prerr_endline x
      | Sys_error x -> prerr_endline x
      | Module.Error x -> prerr_endline x
      end;
      exit 1
  | Error _ -> exit 1
