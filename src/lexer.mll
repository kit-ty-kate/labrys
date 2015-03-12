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

{ exception Error }

let alpha = ['a'-'z' 'A'-'Z']
let term_name = (['a'-'z'] alpha*)
let type_name = (['A'-'Z'] alpha*)
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
  | '|' { Parser.Pipe }
  | '*' { Parser.Star }
  | '\\'
  | "λ" { Parser.Lambda }
  | "->" { Parser.Arrow }
  | "-[" { Parser.LArrowEff }
  | "]->" { Parser.RArrowEff }
  | "let" { Parser.Let }
  | "rec" { Parser.Rec }
  | "in" { Parser.In }
  | "fail" { Parser.Fail }
  | "try" { Parser.Try }
  | "match" { Parser.Match }
  | "with" { Parser.With }
  | "end" { Parser.End }
  | "forall" { Parser.Forall }
  | "type" { Parser.Type }
  | "alias" { Parser.Alias }
  | "import" { Parser.Import }
  | "exception" { Parser.Exception }
  | "begin" blank* '\n'
      { let buffer = Buffer.create 4096 in
        Lexing.new_line lexbuf;
        get_binding buffer lexbuf;
        Parser.Binding (Buffer.contents buffer)
      }
  | "--" { simple_comment lexbuf; main lexbuf }
  | term_name as name { Parser.LowerName name }
  | type_name as name { Parser.UpperName name }
  | eof { Parser.EOF }
  | _ { raise Error }

and get_binding buffer = parse
  | '\n' blank* "end" blank* '\n'
      { Lexing.new_line lexbuf; Lexing.new_line lexbuf }
  | '\n' as lxm { Buffer.add_char buffer lxm;
                  Lexing.new_line lexbuf;
                  get_binding buffer lexbuf
                }
  | eof { raise Error }
  | _ as lxm { Buffer.add_char buffer lxm;
               get_binding buffer lexbuf
             }

and simple_comment = parse
  | '\n' { Lexing.new_line lexbuf }
  | eof { () }
  | _ { simple_comment lexbuf }
