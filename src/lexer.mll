{ exception Error }

rule main = parse
  | [' ' '\t'] { main lexbuf }
  | '\n' { Lexing.new_line lexbuf; main lexbuf }
  | "λ" { Parser.Lambda }
  | '.' { Parser.Dot }
  | ':' { Parser.DoubleDot }
  | "->" { Parser.Arrow }
  | ['a'-'z']+ as name { Parser.TermName name }
  | ['A'-'Z'] ['a'-'z']* as name { Parser.TypeName name }
  | '(' { Parser.LParent }
  | ')' { Parser.RParent }
  | eof { Parser.EOF }
  | _ { raise Error }
