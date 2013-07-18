rule main = parse
  | [' ' '\t' '\n'] { main lexbuf }
  | "λ" { Parser.Lambda }
  | '.' { Parser.Dot }
  | ':' { Parser.DoubleDot }
  | "->" { Parser.Arrow }
  | ['a'-'z']+ as name { Parser.TermName name }
  | ['A'-'Z'] ['a'-'z']* as name { Parser.TypeName name }
  | '(' { Parser.LParent }
  | ')' { Parser.RParent }
  | eof { Parser.EOF }
  | _ { failwith (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf)) }
