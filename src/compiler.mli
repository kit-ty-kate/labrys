(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type module_name = string

val compile_program : Options.program -> module_name -> unit
val compile_module : Options.modul -> module_name -> unit
val print_parse_tree : Options.print_parse_tree -> module_name -> unit
val print_desugared_tree : Options.print_unsugared_tree -> module_name -> unit
val print_untyped_tree : Options.print_untyped_tree -> module_name -> unit
val print_lambda_tree : Options.print_lambda_tree -> module_name -> unit
val print_flatten_tree : Options.print_optimized_tree -> module_name -> unit
val print_optimized_tree : Options.print_optimized_tree -> module_name -> unit
val print_early_llvm : Options.print_early_llvm -> module_name -> unit
val print_llvm : Options.print_llvm -> module_name -> unit
