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

open BatteriesExceptionless
open Monomorphic.None

let start printer lto opt src_dir build_dir lib_dir o modul =
  let options =
    { Options.printer
    ; lto
    ; opt
    ; src_dir
    ; build_dir
    ; lib_dir
    ; o
    }
  in
(*  if lto && c then
    Some
      "Error: Cannot enable the lto optimization while compiling.\n\
       This is allowed only during linking"
*)
  try Compiler.compile options modul; None with
  | Error.Exn x -> Some (Error.dump x)
  | ParserHandler.ParseError x -> Some x
  | Sys_error x -> Some x
  | Llvm_irreader.Error x -> Some x
  | Module.Error x -> Some x

let cmd =
  let module Term = Cmdliner.Term in
  let module Arg = Cmdliner.Arg in
  let ($) = Cmdliner.Term.($) in
  let printers =
    [ (Options.ParseTree, Arg.info ["print-parse-tree"])
    ; (Options.UnsugaredTree, Arg.info ["print-unsugared-tree"])
    ; (Options.TypedTree, Arg.info ["print-typed-tree"])
    ; (Options.UntypedTree, Arg.info ["print-untyped-tree"])
    ; (Options.LLVM, Arg.info ["print-early-llvm"])
    ; (Options.OptimizedLLVM, Arg.info ["print-llvm"])
    ]
  in
  let args = Term.pure start in
  let args = args $ Arg.(value & vflag Options.NoPrinter printers) in
  let args = args $ Arg.(value & flag & info ["lto"]) in
  let args = args $ Arg.(value & opt int 0 & info ["opt"]) in
  let args = args $ Arg.(value & opt dir "" & info ["src-dir"]) in
  let args = args $ Arg.(value & opt dir "dest" & info ["build-dir"]) in
  let args = args $ Arg.(value & opt dir "stdlib" & info ["lib-dir"]) in
  let args = args $ Arg.(value & opt file "a.out" & info ["o"]) in
  let args = args $ Arg.(required & pos 0 (some string) None & info []) in
  (args, Term.info ~version:Config.version "cervoise")

let () =
  match Cmdliner.Term.eval cmd with
  | `Help
  | `Version
  | `Ok None -> exit 0
  | `Ok (Some x) -> prerr_endline x; exit 1
  | `Error _ -> exit 1
