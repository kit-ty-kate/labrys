%token Lambda
%token Dot
%token Arrow
%token DoubleDot
%token <string> TermName
%token <string> TypeName
%token LParent RParent
%token EOF

%left Arrow
%left Lambda Dot
%nonassoc TermName LParent App

%start main
%type <ParseTree.t> main

%%

main:
| term = term EOF { term }

term:
| Lambda termName = TermName DoubleDot typeName = typeExpr Dot term = term
    { ParseTree.Abs ((termName, typeName), term) }
| term1 = term term2 = term %prec App
    { ParseTree.App (term1, term2) }
| termName = TermName
    { ParseTree.Val termName }
| LParent term = term RParent { term }

typeExpr:
| name = TypeName { Types.Ty name }
| t1 = typeExpr Arrow t2 = typeExpr { Types.Fun (t1, t2) }
| LParent term = typeExpr RParent { term }
