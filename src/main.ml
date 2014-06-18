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

open Cmdliner
open BatteriesExceptionless
open Monomorphic.None

let start print_llvm lto opt o file' =
(*  if lto && c then
    Some
      "Error: Cannot enable the lto optimization while compiling.\n\
       This is allowed only during linking"
*)
  let file = ModulePath.of_file file' in
  let modul = ModulePath.to_module file in
  let args = {Compiler.print_llvm; lto; opt; o; file; modul} in
  try Compiler.compile args; None with
  | Error.Exn x -> Some (Error.dump ~file:file' x)
  | Compiler.ParseError x -> Some x
  | Sys_error x -> Some x
  | Llvm_irreader.Error x -> Some x

let cmd =
  let print_llvm = Arg.(value & flag & info ["print-llvm"]) in
  let lto = Arg.(value & flag & info ["lto"]) in
  let opt = Arg.(value & opt int 0 & info ["opt"]) in
  let o = Arg.(value & opt (some string) None & info ["o"]) in
  let file = Arg.(required & pos 0 (some non_dir_file) None & info []) in
  (Term.(pure start $ print_llvm $ lto $ opt $ o $ file), Term.info "cervoise")

let () =
  match Term.eval cmd with
  | `Ok None -> exit 0
  | `Ok (Some x) -> prerr_endline x; exit 1
  | _ -> exit 1
