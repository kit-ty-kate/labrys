module Exn = MonadExn

open MonadStdlib
open Exn.Ops

let aux file =
  let gamma = [] in
  let gammaT = Types.gamma in
  file
  >>= ParserManager.parse
  >>= TypedTree.from_parse_tree gamma gammaT
  >>= fun typed_tree ->
  Exn.return ()

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
    (aux !file)
