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

%token Let Equal
%token Lambda
%token Dot
%token Arrow
%token DoubleDot
%token <string> TermName
%token <string> TypeName
%token LParent RParent
%token EOF

%right Arrow
%left Lambda Dot
%nonassoc TermName LParent
%nonassoc App

%start main
%type <ParseTree.top list> main

%%

main:
| Let name = TermName Equal term = term main = main
   { ParseTree.Value (name, term) :: main }
| EOF { [] }

term:
| Lambda termName = TermName DoubleDot typeName = typeExpr Dot term = term
    { Ast.Abs ((termName, typeName), term) }
| term1 = term term2 = term %prec App
    { Ast.App ((), term1, term2) }
| termName = TermName
    { Ast.Val termName }
| LParent term = term RParent { term }

typeExpr:
| name = TypeName { Ast.Ty name }
| t1 = typeExpr Arrow t2 = typeExpr { Ast.Fun (t1, t2) }
| LParent term = typeExpr RParent { term }
