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

open BatteriesExceptionless
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
    File.with_file_in Filename.get aux

    let parse_impl () = parse Parser.main
    let parse_intf () = parse Parser.mainInterface
end
