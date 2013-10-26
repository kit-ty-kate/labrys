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

rule main = parse
  | [' ' '\t'] { main lexbuf }
  | '\n' { Lexing.new_line lexbuf; main lexbuf }
  | '=' { Parser.Equal }
  | '.' { Parser.Dot }
  | ':' { Parser.DoubleDot }
  | '(' { Parser.LParent }
  | ')' { Parser.RParent }
  | "λ" { Parser.Lambda }
  | "->" { Parser.Arrow }
  | "let" { Parser.Let }
  | "begin" { let buffer = Buffer.create 4096 in
              get_binding buffer lexbuf;
              Parser.Binding (Buffer.contents buffer)
            }
  | term_name as name { Parser.TermName name }
  | type_name as name { Parser.TypeName name }
  | eof { Parser.EOF }
  | _ { raise Error }

and get_binding buffer = parse
  | '\n' as lxm { Buffer.add_char buffer lxm;
                  Lexing.new_line lexbuf;
                  get_binding buffer lexbuf
                }
  | "end" { () }
  | eof { raise Error }
  | _ as lxm { Buffer.add_char buffer lxm;
               get_binding buffer lexbuf
             }
