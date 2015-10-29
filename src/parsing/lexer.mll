(*
Copyright (c) 2013-2015 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

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
  | '!'
  | "φ" { Parser.Eff }
  | '\\'
  | "λ" { Parser.Lambda }
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
