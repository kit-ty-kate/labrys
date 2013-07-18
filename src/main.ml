module Exn = MonadExn

open MonadStdlib
open Exn.Ops

let () =
  let file = ref (failwith (Sys.argv.(0) ^ ": no input file")) in
  let usage =
    "System F ω compiler\n"
    ^ "Usage: " ^ Sys.argv.(0) ^ " file.sfw\n"
    ^ "Options are:"
  in
  Arg.parse
    []
    (fun filename -> file := open_in filename)
    usage;
  Exn.run
    (function
      | `Failure s -> Unsafe.prerr_endline s
      | `NotFound -> Unsafe.prerr_endline "Unknown identifier"
      | `SysError err -> Unsafe.prerr_endline err
    )
    (!file >>= fun file ->
     let filebuf = Lexing.from_channel file in
     let parse_tree = Parser.main Lexer.main filebuf in
     TypedTree.from_parse_tree [] parse_tree >>= fun typed_tree ->
     Exn.return ()
    )
