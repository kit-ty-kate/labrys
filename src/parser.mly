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
%token Forall
%token Datatype
%token Pipe
%token DoubleDot
%token <string> TermName
%token <string> TypeName
%token <string> Binding
%token LParent RParent
%token LBracket RBracket
%token EOF

%left Lambda Dot
%right Arrow
%nonassoc TermName TypeName LParent LBracket
%nonassoc app

%start main
%type <ParseTree.top list> main

%%

main:
| Let name = TermName Equal term = term main = main
   { ParseTree.Value (name, term) :: main }
| Let name = TypeName Equal ty = typeExpr main = main
   { ParseTree.Type (name, ty) :: main }
| Let name = TermName DoubleDot ty = typeExpr Equal binding = Binding main = main
   { ParseTree.Binding (name, ty, binding) :: main }
| Datatype name = TypeName Equal option(Pipe) variants = separated_nonempty_list(Pipe, variant) main = main
   { ParseTree.Datatype (name, variants) :: main }
| EOF { [] }

term:
| Lambda termName = TermName DoubleDot typeName = typeExpr Dot term = term
    { ParseTree.Abs ((termName, typeName), term) }
| Lambda typeName = TypeName Dot term = term
    { ParseTree.TAbs (typeName, term) }
| term1 = term term2 = term %prec app
    { ParseTree.App (term1, term2) }
| term1 = term LBracket ty = typeExpr RBracket
    { ParseTree.TApp (term1, ty) }
| termName = TermName
| termName = TypeName
    { ParseTree.Val termName }
| LParent term = term RParent { term }

typeExpr:
| name = TypeName { ParseTree.Ty name }
| param = typeExpr Arrow ret = typeExpr { ParseTree.Fun (param, ret) }
| Forall ty = TypeName Dot ret = typeExpr { ParseTree.Forall (ty, ret) }
| LParent term = typeExpr RParent { term }

variant:
| name = TypeName DoubleDot ty = typeExpr { ParseTree.Variant (name, ty) }
