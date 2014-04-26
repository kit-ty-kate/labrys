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

let sprintf = Printf.sprintf

type args =
  { print : bool
  ; lto : bool
  ; opt : int
  ; c : bool
  ; o : string option
  ; file : string
  }

let print_error () =
  prerr_endline "\nThe compilation processes exited abnormally"

let link ~tmp ~o =
    let tmp = Filename.quote tmp in
    let o = Filename.quote o in
    let ld = Sys.command (sprintf "cc %s -o %s" tmp o) in
    if Int.(ld <> 0) then begin
      print_error ();
    end

let with_tmp_file f =
  let tmp = Filename.temp_file "cervoise" "" in
  f tmp;
  Sys.remove tmp

let compile {c; o; file; _} result =
  let o = match o with
    | Some o -> o
    | None -> if c then Utils.replace_ext file "bc" else "a.out"
  in
  if c then begin
    if not (Llvm_bitwriter.write_bitcode_file result o) then
      print_error ()
  end else begin
    Llvm_backends.initialize ();
    let triple = Llvm_target.Target.default_triple () in
    let target = Llvm_target.Target.by_triple triple in
    let target = Llvm_target.TargetMachine.create ~triple target in
    let aux tmp =
      Llvm_target.TargetMachine.emit_to_file
        result
        Llvm_target.CodeGenFileType.ObjectFile
        tmp
        target;
      link ~tmp ~o
    in
    with_tmp_file aux;
  end

let print_or_compile = function
  | {print = true; _} -> print_endline % Llvm.string_of_llmodule
  | {print = false; _} as args -> compile args

let aux args =
  open_in args.file
  |> ParserManager.parse
  |> TypeChecker.from_parse_tree
  |> Lambda.of_typed_tree
  |> Backend.make ~with_main:(not args.c) ~lto:args.lto ~opt:args.opt
  |> print_or_compile args

let start print lto opt c o file =
  try aux {print; lto; opt; c; o; file}; None with
  | Error.Exn x -> Some (Error.dump ~file x)
  | ParserManager.Error x -> Some x

let cmd =
  let print = Arg.(value & flag & info ["print"]) in
  let lto = Arg.(value & flag & info ["lto"]) in
  let opt = Arg.(value & opt int 0 & info ["opt"]) in
  let c = Arg.(value & flag & info ["c"]) in
  let o = Arg.(value & opt (some string) None & info ["o"]) in
  let file = Arg.(required & pos 0 (some non_dir_file) None & info []) in
  (Term.(pure start $ print $ lto $ opt $ c $ o $ file), Term.info "cervoise")

let () =
  match Term.eval cmd with
  | `Ok None -> exit 0
  | `Ok (Some x) -> prerr_endline x; exit 1
  | _ -> exit 1
