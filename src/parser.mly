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

%{
  let get_loc startpos endpos =
    let get_pos pos =
      { ParseTree.pos_lnum = pos.Lexing.pos_lnum
      ; pos_cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol
      }
    in
    { ParseTree.loc_start = get_pos startpos
    ; loc_end = get_pos endpos
    }
%}

%token Let Equal
%token Lambda
%token Dot
%token Arrow
%token Forall
%token Datatype
%token Pipe
%token DoubleDot
%token Star
%token <string> TermName
%token <string> TypeName
%token <string> Binding
%token LParent RParent
%token LBracket RBracket
%token EOF

%left Lambda Dot Forall
%right Arrow
%nonassoc TermName TypeName LParent LBracket
%nonassoc app tapp

%start main
%type <ParseTree.top list> main

%%

main:
| Let name = TermName Equal term = term main = main
    { ParseTree.Value (name, term) :: main }
| Let name = TypeName Equal ty = typeExpr main = main
    { ParseTree.Type (get_loc $startpos $endpos(ty), name, ty) :: main }
| Let name = TermName DoubleDot ty = typeExpr Equal binding = Binding main = main
    { ParseTree.Binding (get_loc $startpos $endpos(binding), name, ty, binding) :: main }
| Datatype name = TypeName k = kindopt Equal option(Pipe) variants = separated_nonempty_list(Pipe, variant) main = main
    { ParseTree.Datatype (get_loc $startpos $endpos(variants), name, k, variants) :: main }
| EOF
    { [] }

term:
| Lambda termName = TermName DoubleDot typeName = typeExpr Dot term = term
    { ParseTree.Abs (get_loc $startpos $endpos(typeName), (termName, typeName), term) }
| Lambda typeName = TypeName k = kindopt Dot term = term
    { ParseTree.TAbs (get_loc $startpos $endpos(k), (typeName, k), term) }
| term1 = term term2 = term %prec app
    { ParseTree.App (get_loc $startpos $endpos(term2), term1, term2) }
| term1 = term LBracket ty = typeExpr RBracket
    { ParseTree.TApp (get_loc $startpos $endpos, term1, ty) }
| termName = TermName
| termName = TypeName
    { ParseTree.Val (get_loc $startpos $endpos, termName) }
| LParent term = term RParent
    { term }

typeExpr:
| name = TypeName
    { ParseTree.Ty name }
| param = typeExpr Arrow ret = typeExpr
    { ParseTree.Fun (param, ret) }
| Forall ty = TypeName k = kindopt Dot ret = typeExpr
    { ParseTree.Forall (ty, k, ret) }
| Lambda name = TypeName k = kindopt Dot ret = typeExpr
    { ParseTree.AbsOnTy (name, k, ret) }
| f = typeExpr x = typeExpr %prec tapp
    { ParseTree.AppOnTy (f, x) }
| LParent term = typeExpr RParent
    { term }

kind:
| Star
    { Kinds.Star }
| k1 = kind Arrow k2 = kind
    { Kinds.KFun (k1, k2) }
| LParent k = kind RParent
    { k }

variant:
| name = TypeName DoubleDot ty = typeExpr
    { ParseTree.Variant (get_loc $startpos $endpos, name, ty) }

kindopt:
| { Kinds.Star }
| DoubleDot k = kind { k }
