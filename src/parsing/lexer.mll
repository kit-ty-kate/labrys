(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

{
module Make (Filename : sig val get : string end) = struct
  exception Error

  module Parser = Parser.Make(Filename)
}

let prime = '\''
let alpha = ['a'-'z' 'A'-'Z' '0'-'9']
let num = ['0'-'9']
(*let hexa = [num 'a'-'f' 'A'-'F']*)
let term_name = (['a'-'z'] alpha* prime*)
let type_name = (['A'-'Z'] alpha* prime*)
let int = num+
let float = (num+ '.' num+)
let blank = [' ' '\t']

rule main = parse
  | blank { main lexbuf }
  | '\n' { Lexing.new_line lexbuf; main lexbuf }
  | '=' { Parser.Equal }
  | '.' { Parser.Dot }
  | '_' { Parser.Underscore }
  | ',' { Parser.Comma }
  | ':' { Parser.Colon }
  | ';' { Parser.Semicolon }
  | '(' { Parser.LParen }
  | ')' { Parser.RParen }
  | '[' { Parser.LBracket }
  | ']' { Parser.RBracket }
  | '{' { Parser.LBrace }
  | '}' { Parser.RBrace }
  | '|' { Parser.Pipe }
  | '*' { Parser.Star }
  | '#' { Parser.Hash }
  | '^' { Parser.Caret }
  | '!'
  | "φ" { Parser.Eff }
  | '\\'
  | "λ" { Parser.Lambda }
  | "[^" { Parser.LBracketUp }
  | "^]" { Parser.RBracketUp }
  | "?[" { Parser.LQMarkBracket }
  | "?(" { Parser.LQMarkParen }
  | "->" { Parser.Arrow }
  | "=>" { Parser.DoubleArrow }
  | "-[" { Parser.LArrowEff }
  | "=[" { Parser.LDoubleArrowEff }
  | "]->" { Parser.RArrowEff }
  | "]=>" { Parser.RDoubleArrowEff }
  | "let" { Parser.Let }
  | "rec" { Parser.Rec }
  | "in" { Parser.In }
  | "as" { Parser.As }
  | "fail" { Parser.Fail }
  | "try" { Parser.Try }
  | "match" { Parser.Match }
  | "with" { Parser.With }
  | "end" { Parser.End }
  | "∀"
  | "forall" { Parser.Forall }
  | "type" { Parser.Type }
  | "alias" { Parser.Alias }
  | "class" { Parser.Class }
  | "open" { Parser.Open }
  | "import" { Parser.Import }
  | "library" { Parser.Library }
  | "foreign" { Parser.Foreign }
  | "instance" { Parser.Instance }
  | "exception" { Parser.Exception }
  | "--" { simple_comment lexbuf; main lexbuf }
  | term_name as name { Parser.LowerName name }
  | type_name as name { Parser.UpperName name }
  | int as n { Parser.Int n }
  | float as n { Parser.Float n }
  | '\'' { Parser.Char (get_char lexbuf) }
  | '"' { Parser.String (get_string lexbuf) }
  | eof { Parser.EOF }
  | _ { raise Error }

and simple_comment = parse
  | '\n' { Lexing.new_line lexbuf }
  | eof { () }
  | _ { simple_comment lexbuf }

and get_char = parse
  | "\\'" { '\'' :: get_char lexbuf }
  | '\'' { [] }
  | _ as c { c :: get_char lexbuf }
  | eof { raise Error }

and get_string = parse
  | "\\\"" { '\"' :: get_string lexbuf }
  | '"' { [] }
  | _ as c { c :: get_string lexbuf }
  | eof { raise Error }

{ end }
