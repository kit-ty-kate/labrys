(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open Containers
open Monomorphic.None

let fmt = Printf.sprintf

exception ParseError of string

module Make (Filename : sig val get : string end) = struct
  module Parser = Parser.Make(Filename)
  module Lexer = Lexer.Make(Filename)

  let parse parser =
    let aux file =
      let filebuf = Lexing.from_channel file in
      let get_offset () =
        let pos = Lexing.lexeme_start_p filebuf in
        let open Lexing in
        let column = pos.pos_cnum - pos.pos_bol in
        string_of_int pos.pos_lnum ^ ":" ^ string_of_int column
      in
      try parser Lexer.main filebuf with
      | Lexer.Error ->
          raise
            (ParseError (fmt "%s: Lexing error at: %s" Filename.get (get_offset ())))
      | Parser.Error ->
          raise
            (ParseError (fmt "%s: Parsing error at: %s" Filename.get (get_offset ())))
    in
    Utils.CCIO.with_in Filename.get aux

    let parse_impl () = parse Parser.main
    let parse_intf () = parse Parser.mainInterface
end
