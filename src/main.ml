let () =
  let file = ref None in
  let usage =
    "System F ω compiler\n"
    ^ "Usage: " ^ Sys.argv.(0) ^ " file.sfw\n"
    ^ "Options are:"
  in
  Arg.parse
    []
    (fun filename -> file := Some (open_in filename))
    usage;
  match !file with
    | Some file ->
        let filebuf = Lexing.from_channel file in
        let parse_tree = Parser.main Lexer.main filebuf in
        let typed_tree = TypedTree.from_parse_tree [] parse_tree in
        ()
    | None ->
        prerr_endline (Sys.argv.(0) ^ ": no input file")
