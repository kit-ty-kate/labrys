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
open MonadOpen

let compile result =
  let output =
    Unix.open_process_out "llc - | cc -x assembler - -o a.out"
  in
  output_string output result >>= fun () ->
  match Unix.close_process_out output with
    | Unix.WEXITED 0 -> Exn.return ()
    | _ -> prerr_endline "\nThe compilation processes exited abnormally"

let aux file =
  let gamma = [] in
  let gammaT = Types.gamma in
  open_in file
  >>= ParserManager.parse
  >>= TypedTree.from_parse_tree gamma gammaT
  >>= Backend.from_typed_tree
  >|= Backend.to_string
  >>= compile
  >> Exn.return None

let start file =
  let catch = function
    | `Failure s -> Some s
    | `NotFound -> Some "Unknown identifier"
    | `SysError err -> Some err
  in
  Exn.run catch (aux file)

let cmd =
  let file = Arg.(required & pos 0 (some non_dir_file) None & info []) in
  (Term.(pure start $ file), Term.info "cervoise")

let () =
  match Term.eval cmd with
    | `Ok None -> exit 0
    | `Ok (Some x) -> Unsafe.prerr_endline x; exit 1
    | _ -> exit 1
