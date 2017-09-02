(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type module_name = string

val compile_program : module_name -> Options.program -> unit
val compile_module : module_name -> Options.modul -> unit
val print_parse_tree : module_name -> Options.print_parse_tree -> unit
val print_desugared_tree : module_name -> Options.print_desugared_tree -> unit
val print_untyped_tree : module_name -> Options.print_untyped_tree -> unit
val print_lambda_tree : module_name -> Options.print_lambda_tree -> unit
val print_flatten_tree : module_name -> Options.print_optimized_tree -> unit
val print_optimized_tree : module_name -> Options.print_optimized_tree -> unit
val print_early_llvm : module_name -> Options.print_early_llvm -> unit
val print_llvm : module_name -> Options.print_llvm -> unit
