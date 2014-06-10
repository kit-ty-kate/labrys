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
      { Location.pos_lnum = pos.Lexing.pos_lnum
      ; pos_cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol
      }
    in
    { Location.loc_start = get_pos startpos
    ; loc_end = get_pos endpos
    }

  let let_lambda_sugar term args =
    let aux term = function
      | `Val (loc, arg) -> ParseTree.Abs (loc, arg, term)
      | `Ty (loc, ty) -> ParseTree.TAbs (loc, ty, term)
    in
    List.fold_left aux term (List.rev args)

  let let_rec_lambda_sugar term ty args =
    let aux (term, ty) = function
      | `Val (loc, ((_, ty') as arg)) ->
          (ParseTree.Abs (loc, arg, term), ParseTree.Fun (ty', ty))
      | `Ty (loc, ty') ->
          (ParseTree.TAbs (loc, ty', term), ParseTree.Forall (ty', ty))
    in
    List.fold_left aux (term, ty) (List.rev args)

  let param_ty_sugar f values ret =
    let aux ret value = f (value, ret) in
    List.fold_left aux ret (List.rev values)
%}

%token Import
%token Let Equal In
%token Rec
%token Lambda
%token Dot
%token Comma
%token Arrow
%token Forall
%token Match With End
%token Type
%token Alias
%token Pipe
%token Colon
%token Star
%token <string> LowerName
%token <string> UpperName
%token <string> Binding
%token LParen RParen
%token LBracket RBracket
%token EOF

%left Lambda Comma Forall Match Let In
%right Arrow
%nonassoc LowerName UpperName LParen LBracket
%nonassoc app tapp

%start main
%type <(ParseTree.imports * ParseTree.top list)> main

%start mainInterface
%type <(ParseTree.imports * Interface.t list)> mainInterface

%%

entry(body):
| imports = list(import) body = body
    { (imports, body) }

import:
| Import modul = module_name
    { modul }

module_name:
| name = upperName
    { Gamma.Type.of_list name }

(********* Implementation *********)

main:
| x = entry(body)
    { x }

body:
| Let name = LowerName args = list(arg) Equal term = term xs = body
    { let term = let_lambda_sugar term args in
      ParseTree.Value (Gamma.Name.of_list [name], term) :: xs
    }
| Let Rec name = LowerName args = list(arg) Colon ty = typeExpr Equal term = term xs = body
    { let (term, ty) = let_rec_lambda_sugar term ty args in
      ParseTree.RecValue
        (get_loc $startpos $endpos(term), Gamma.Name.of_list [name], ty, term)
      :: xs
    }
| typeAlias = typeAlias xs = body
    { ParseTree.Type typeAlias :: xs }
| Let name = LowerName Colon ty = typeExpr Equal binding = Binding xs = body
    { ParseTree.Binding
        (get_loc $startpos $endpos(binding), Gamma.Name.of_list [name], ty, binding)
      :: xs
    }
| datatype = datatype xs = body
    { ParseTree.Datatype datatype :: xs }
| EOF
    { [] }

datatype:
| Type name = UpperName k = kindopt Equal option(Pipe) variants = separated_nonempty_list(Pipe, variant)
    { (get_loc $startpos $endpos, Gamma.Type.of_list [name], k, variants) }

typeAlias:
| Type Alias name = UpperName Equal ty = typeExpr
    { (get_loc $startpos $endpos, Gamma.Type.of_list [name], ty) }

term:
| Lambda args = nonempty_list(arg) Arrow term = term
    { let_lambda_sugar term args }
| term1 = term term2 = term %prec app
    { ParseTree.App (get_loc $startpos $endpos(term2), term1, term2) }
| term1 = term LBracket ty = typeExpr RBracket
    { ParseTree.TApp (get_loc $startpos $endpos, term1, ty) }
| name = name
    { ParseTree.Val (get_loc $startpos $endpos, Gamma.Name.of_list name) }
| LParen term = term RParen
    { term }
| Match t = term With option(Pipe) p = separated_nonempty_list(Pipe, pattern) End
    { ParseTree.PatternMatching (get_loc $startpos $endpos, t, p) }
| Let name = LowerName args = list(arg) Equal t = term In xs = term
    { let t = let_lambda_sugar t args in
      ParseTree.Let (Gamma.Name.of_list [name], t, xs)
    }
| Let Rec name = LowerName args = list(arg) Colon ty = typeExpr Equal t = term In xs = term
    { let (t, ty) = let_rec_lambda_sugar t ty args in
      ParseTree.LetRec
        (get_loc $startpos $endpos(ty), Gamma.Name.of_list [name], ty, t, xs)
    }

arg:
| LParen name = LowerName Colon ty = typeExpr RParen
    { `Val (get_loc $startpos $endpos, (Gamma.Name.of_list [name], ty)) }
| ty = kind_and_name
    { `Ty (get_loc $startpos $endpos, ty) }

name:
| name = LowerName
| name = UpperName
    { [name] }
| m = UpperName Dot xs = name
    { m :: xs }

upperName:
| name = UpperName
    { [name] }
| m = UpperName Dot xs = upperName
    { m :: xs }

typeExpr:
| name = upperName
    { ParseTree.Ty (Gamma.Type.of_list name) }
| param = typeExpr Arrow ret = typeExpr
    { ParseTree.Fun (param, ret) }
| Forall values = nonempty_list(kind_and_name) Comma ret = typeExpr
    { param_ty_sugar (fun x -> ParseTree.Forall x) values ret }
| Lambda values = nonempty_list(kind_and_name) Comma ret = typeExpr
    { param_ty_sugar (fun x -> ParseTree.AbsOnTy x) values ret }
| f = typeExpr x = typeExpr %prec tapp
    { ParseTree.AppOnTy (f, x) }
| LParen term = typeExpr RParen
    { term }

kind:
| Star
    { Kinds.Star }
| k1 = kind Arrow k2 = kind
    { Kinds.KFun (k1, k2) }
| LParen k = kind RParen
    { k }

variant:
| name = UpperName Colon ty = typeExpr
    { ParseTree.Variant
        (get_loc $startpos $endpos, Gamma.Name.of_list [name], ty)
    }

kindopt:
| { Kinds.Star }
| Colon k = kind { k }

kind_and_name:
| name = UpperName
    { (Gamma.Type.of_list [name], Kinds.Star) }
| LParen name = UpperName Colon k = kind RParen
    { (Gamma.Type.of_list [name], k) }

pattern:
| p = pat Arrow t = term { (p, t) }

pat:
| name = LowerName
    { ParseTree.Any (Gamma.Name.of_list [name]) }
| name = upperName args = list(pat_arg)
    { ParseTree.TyConstr (Gamma.Name.of_list name, args) }

pat_arg:
| name = LowerName
    { ParseTree.PVal (ParseTree.Any (Gamma.Name.of_list [name])) }
| name = upperName
    { ParseTree.PVal (ParseTree.TyConstr (Gamma.Name.of_list name, [])) }
| LParen p = pat RParen
    { ParseTree.PVal p }
| LBracket ty = typeExpr RBracket
    { ParseTree.PTy ty }

(********* Interface *********)

mainInterface:
| x = entry(bodyInterface)
    { x }

bodyInterface:
| Let name = LowerName Colon ty = typeExpr xs = bodyInterface
    { Interface.Val
        (get_loc $startpos $endpos(ty), Gamma.Name.of_list [name], ty)
      :: xs
    }
| Type name = UpperName k = kindopt xs = bodyInterface
    { Interface.AbstractType
        (get_loc $startpos $endpos(name), (Gamma.Type.of_list [name], k))
      :: xs
    }
| datatype = datatype xs = bodyInterface
    { Interface.Datatype datatype :: xs }
| typeAlias = typeAlias xs = bodyInterface
    { Interface.TypeAlias typeAlias :: xs }
| EOF
    { [] }
