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
open Batteries
open MonadOpen
open Monomorphic.None

let sprintf = Printf.sprintf

type args = {print : bool; c : bool; o : string option; file : string}

let compile {c; o; file; _} result =
  let o = match o with
    | Some o -> o
    | None -> if c then Utils.replace_ext file "o" else "a.out"
  in
  let c = if c then "-c" else "" in
  let o = Filename.quote o in
  let command = sprintf "llc - | cc %s -x assembler - -o %s" c o in
  let output = Legacy.Unix.open_process_out command in
  output_string output result >>= fun () ->
  match Legacy.Unix.close_process_out output with
  | Unix.WEXITED 0 -> Exn.return ()
  | _ -> prerr_endline "\nThe compilation processes exited abnormally"

let print_or_compile = function
  | {print = true; _} -> print_endline
  | {print = false; _} as args -> compile args

let aux args =
  let gamma = Gamma.values in
  let gammaT = Gamma.types in
  open_in args.file
  >>= ParserManager.parse
  >>= TypedTree.from_parse_tree gamma gammaT
  >|= UntypedTree.of_typed_tree
  >>= Backend.make
  >|= LLVM.to_string
  >>= print_or_compile args
  >> Exn.return None

let start print c o file =
  let catch = function
    | `Failure s -> Some s
    | `NotFound -> Some "Unknown identifier"
    | `SysError err -> Some err
  in
  Exn.run catch (aux {print; c; o; file})

let cmd =
  let print = Arg.(value & flag & info ["print"]) in
  let c = Arg.(value & flag & info ["c"]) in
  let o = Arg.(value & opt (some string) None & info ["o"]) in
  let file = Arg.(required & pos 0 (some non_dir_file) None & info []) in
  (Term.(pure start $ print $ c $ o $ file), Term.info "cervoise")

let () =
  match Term.eval cmd with
  | `Ok None -> exit 0
  | `Ok (Some x) -> Unsafe.prerr_endline x; exit 1
  | _ -> exit 1
