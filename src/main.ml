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

let compile {c; o; file; _} result =
  let o = match o with
    | Some o -> o
    | None -> if c then Utils.replace_ext file "o" else "a.out"
  in
  let c = if c then "-c" else "" in
  let o = Filename.quote o in
  let command = sprintf "llc - | cc %s -x assembler - -o %s" c o in
  let output = Unix.open_process_out command in
  output_string output result;
  match Unix.close_process_out output with
  | Unix.WEXITED 0 -> ()
  | _ -> prerr_endline "\nThe compilation processes exited abnormally"

let print_or_compile = function
  | {print = true; _} -> print_endline
  | {print = false; _} as args -> compile args

let aux args =
  open_in args.file
  |> ParserManager.parse
  |> TypeChecker.from_parse_tree
  |> Lambda.of_typed_tree
  |> Backend.make ~with_main:(not args.c) ~lto:args.lto ~opt:args.opt
  |> Llvm.string_of_llmodule
  |> print_or_compile args

let start print lto opt c o file =
  if lto && c then
    Some
      "Error: Cannot enable the lto optimization while compiling.\n\
       This is allowed only during linking"
  else
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
