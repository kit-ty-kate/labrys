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

let start printer lto opt o file =
(*  if lto && c then
    Some
      "Error: Cannot enable the lto optimization while compiling.\n\
       This is allowed only during linking"
*)
  try Compiler.compile ~printer ~lto ~opt ~o file; None with
  | Error.Exn x -> Some (Error.dump ~file x)
  | Compiler.ParseError x -> Some x
  | Sys_error x -> Some x
  | Llvm_irreader.Error x -> Some x

let cmd =
  let module Term = Cmdliner.Term in
  let module Arg = Cmdliner.Arg in
  let ($) = Cmdliner.Term.($) in
  let printers =
    [ (Compiler.ParseTree, Arg.info ["print-parse-tree"])
    ; (Compiler.TypedTree, Arg.info ["print-typed-tree"])
    ; (Compiler.UntypedTree, Arg.info ["print-untyped-tree"])
    ; (Compiler.LLVM, Arg.info ["print-early-llvm"])
    ; (Compiler.OptimizedLLVM, Arg.info ["print-llvm"])
    ]
  in
  let args = Term.pure start in
  let args = args $ Arg.(value & vflag Compiler.NoPrinter printers) in
  let args = args $ Arg.(value & flag & info ["lto"]) in
  let args = args $ Arg.(value & opt int 0 & info ["opt"]) in
  let args = args $ Arg.(value & opt (some string) None & info ["o"]) in
  let args = args $ Arg.(required & pos 0 (some non_dir_file) None & info []) in
  (args, Term.info "cervoise")

let () =
  match Cmdliner.Term.eval cmd with
  | `Ok None -> exit 0
  | `Ok (Some x) -> prerr_endline x; exit 1
  | _ -> exit 1
