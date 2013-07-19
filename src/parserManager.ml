module Exn = MonadExn

open MonadStdlib
open Exn.Ops

let parse file =
  let filebuf = Lexing.from_channel file in
  let get_offset () =
    let pos = Lexing.lexeme_start_p filebuf in
    let open Lexing in
    let column = pos.pos_cnum - pos.pos_bol in
    string_of_int pos.pos_lnum ^ ":" ^ string_of_int column
  in
  try
    Exn.return (Parser.main Lexer.main filebuf)
  with
    | Lexer.Error -> failwith ("Lexing error at: " ^ get_offset ())
    | Parser.Error -> failwith ("Parsing error at: " ^ get_offset ())
