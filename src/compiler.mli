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

type module_name = string

val compile_program : Options.program -> module_name -> unit
val compile_module : Options.modul -> module_name -> unit
val print_parse_tree : Options.print_parse_tree -> module_name -> unit
val print_unsugared_tree : Options.print_unsugared_tree -> module_name -> unit
val print_untyped_tree : Options.print_untyped_tree -> module_name -> unit
val print_lambda_tree : Options.print_lambda_tree -> module_name -> unit
val print_optimized_tree : Options.print_optimized_tree -> module_name -> unit
val print_early_llvm : Options.print_early_llvm -> module_name -> unit
val print_llvm : Options.print_llvm -> module_name -> unit
