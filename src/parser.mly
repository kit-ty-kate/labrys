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
%}

%token Import
%token Let Equal In
%token Rec
%token Lambda
%token Dot
%token Comma
%token Arrow
%token LArrowEff RArrowEff
%token Forall
%token Match With End
%token Type
%token Alias
%token Pipe
%token Colon
%token Star
%token Fail
%token Try
%token Exception
%token <string> LowerName
%token <string> UpperName
%token <string> Binding
%token LParen RParen
%token LBracket RBracket
%token EOF

%start main
%type <(ParseTree.imports * ParseTree.top list)> main

%start mainInterface
%type <(ParseTree.imports * Interface.t list)> mainInterface

%%

(********* Main functions *********)

main: entry(body) { $1 }

mainInterface: entry(bodyInterface) { $1 }


(********* Implementation *********)

body:
  | x = let_case
      { ParseTree.Value x }
  | typeAlias = typeAlias
      { ParseTree.Type typeAlias }
  | Let name = LowerName Colon ty = typeExpr Equal binding = Binding
      { ParseTree.Binding (Ident.Name.of_list [name], ty, binding) }
  | datatype = datatype
      { ParseTree.Datatype datatype }
  | Exception name = UpperName args = exceptionArgs
      { ParseTree.Exception (Ident.Exn.of_list [name], args) }

exceptionArgs:
  | x = typeExprProtected xs = exceptionArgs
      { x :: xs }
  | { [] }

let_case:
  | Let Rec name = LowerName t = let_sugar
      { (Ident.Name.of_list [name], ParseTree.Rec, t) }
  | Let name = LowerName t = let_sugar
      { (Ident.Name.of_list [name], ParseTree.NonRec, t) }

datatype:
  | Type name = UpperName k = kindopt Equal option(Pipe) variants = separated_nonempty_list(Pipe, variant)
      { (Ident.Type.of_list [name], k, variants) }

typeAlias:
  | Type Alias name = UpperName Equal ty = typeExpr
      { (Ident.Type.of_list [name], ty) }

termUnclosed:
  | Lambda x = lambda_sugar
      { x }
  | x = app
      { x }
  | x = let_case In xs = term
      { ParseTree.Let (x, xs) }
  | Fail LBracket ty = typeExpr RBracket exn = effectValue
      { ParseTree.Fail (ty, exn) }

termClosed:
  | name = name
      { ParseTree.Val (Ident.Name.of_list name) }
  | Match t = term With option(Pipe) p = separated_nonempty_list(Pipe, pattern) End
      { ParseTree.PatternMatching (t, p) }
  | Try t = term With option(Pipe) p = separated_nonempty_list(Pipe, exn_pattern) End
      { ParseTree.Try (t, p) }

term:
  | x = termUnclosed { (get_loc $startpos $endpos, x) }
  | x = termClosed { (get_loc $startpos $endpos, x) }

app:
  | f = termProtected LBracket ty = typeExpr RBracket
      { ParseTree.TApp (f, ty) }
  | f = app LBracket ty = typeExpr RBracket
      { ParseTree.TApp ((get_loc $startpos(f) $endpos(f), f), ty) }
  | f = termProtected x = termProtected
      { ParseTree.App (f, x) }
  | f = app x = termProtected
      { ParseTree.App ((get_loc $startpos(f) $endpos(f), f), x) }

arg:
  | LParen name = LowerName Colon ty = typeExpr RParen
      { (Ident.Name.of_list [name], ty) }

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

typeExprUnclosed:
  | param = typeExprProtected Arrow ret = typeExpr
      { ParseTree.Fun (param, [], ret) }
  | param = typeExprProtected LArrowEff eff = eff RArrowEff ret = typeExpr
      { ParseTree.Fun (param, eff, ret) }
  | Forall x = forall_ty_sugar
      { x }
  | Lambda x = lambda_ty_sugar
      { x }
  | x = tyApp
      { x }

typeExprClosed:
  | name = upperName
      { ParseTree.Ty (Ident.Type.of_list name) }

typeExpr:
  | x = typeExprUnclosed { (get_loc $startpos $endpos, x) }
  | x = typeExprClosed { (get_loc $startpos $endpos, x) }

tyApp:
  | f = typeExprProtected x = typeExprProtected
      { ParseTree.AppOnTy (f, x) }
  | f = tyApp x = typeExprProtected
      { ParseTree.AppOnTy ((get_loc $startpos(f) $endpos(f), f), x) }

kindUnclosed:
  | k1 = kindProtected Arrow k2 = kind
      { Kinds.KFun (k1, k2) }

kindClosed:
  | Star
      { Kinds.Star }

kind:
  | x = kindUnclosed { x }
  | x = kindClosed { x }

eff: eff = separated_list(Comma, effectName) { eff }

effectName:
  | name = UpperName
      { (Ident.Eff.of_list [name], []) }
  | name = UpperName LBracket args = eff_exn RBracket
      { (Ident.Eff.of_list [name], args) }

eff_exn:
  | name = upperName
      { [Ident.Exn.of_list name] }
  | name = upperName Pipe xs = eff_exn
      { Ident.Exn.of_list name :: xs }

effectValue:
  | LParen name = upperName args = effectValueArgs RParen
      { (Ident.Exn.of_list name, args) }
  | name = upperName
      { (Ident.Exn.of_list name, []) }

effectValueArgs:
  | x = termProtected xs = effectValueArgs
      { x :: xs }
  | x = termProtected
      { [x] }

exn_pattern:
  | exn = upperName args = list(exn_pattern_arg) Arrow t = term
      { ((Ident.Exn.of_list exn, args), t) }

exn_pattern_arg:
  | name = LowerName { Ident.Name.of_list [name] }

variant:
  | name = UpperName Colon ty = typeExpr
      { ParseTree.Variant
          (get_loc $startpos $endpos, Ident.Name.of_list [name], ty)
      }

kindopt:
  | { Kinds.Star }
  | Colon k = kind { k }

kind_and_name:
  | name = UpperName
      { (Ident.Type.of_list [name], Kinds.Star) }
  | LParen name = UpperName Colon k = kind RParen
      { (Ident.Type.of_list [name], k) }

pattern:
  | p = pat Arrow t = term
      { (p, t) }

pat:
  | name = LowerName
      { ParseTree.Any (Ident.Name.of_list [name]) }
  | name = upperName args = list(pat_arg)
      { ParseTree.TyConstr
          (get_loc $startpos $endpos, Ident.Name.of_list name, args)
      }

pat_arg:
  | name = LowerName
      { ParseTree.PVal (ParseTree.Any (Ident.Name.of_list [name])) }
  | name = upperName
      { ParseTree.PVal
          (ParseTree.TyConstr
             (get_loc $startpos $endpos, Ident.Name.of_list name, [])
          )
      }
  | LParen p = pat RParen
      { ParseTree.PVal p }
  | LBracket ty = typeExpr RBracket
      { ParseTree.PTy ty }


(********* Interface *********)

bodyInterface:
  | Let name = LowerName Colon ty = typeExpr
      { Interface.Val (Ident.Name.of_list [name], ty) }
  | Type name = UpperName k = kindopt
      { Interface.AbstractType (Ident.Type.of_list [name], k) }
  | datatype = datatype
      { Interface.Datatype datatype }
  | typeAlias = typeAlias
      { Interface.TypeAlias typeAlias }
  | Exception name = UpperName args = exceptionArgs
      { Interface.Exception (Ident.Exn.of_list [name], args) }



(********* Syntactic sugars *********)

lambda_ty_sugar:
  | v = kind_and_name Comma ret = typeExpr
      { ParseTree.AbsOnTy (v, ret) }
  | v = kind_and_name xs = lambda_ty_sugar
      { ParseTree.AbsOnTy (v, (get_loc $startpos(xs) $endpos(xs), xs)) }

forall_ty_sugar:
  | v = kind_and_name Comma ret = typeExpr
      { ParseTree.Forall (v, ret) }
  | v = kind_and_name xs = forall_ty_sugar
      { ParseTree.Forall (v, (get_loc $startpos(xs) $endpos(xs), xs)) }

let_sugar:
  | Colon LBracket eff = eff RBracket ty = typeExpr Equal t = term
      { (Some (ty, eff), t) }
  | Colon ty = typeExpr Equal t = term
      { (Some (ty, []), t) }
  | Equal t = term
      { (None, t) }
  | arg = arg xs = let_sugar
      { let (ty_xs, xs) = xs in
        let ty_xs =
          let aux (ty_xs, eff) =
            let ty_xs = ParseTree.Fun (snd arg, eff, ty_xs) in
            ((get_loc $startpos(arg) $endpos(arg), ty_xs), [])
          in
          BatOption.map aux ty_xs
        in
        (ty_xs, (get_loc $startpos $endpos, ParseTree.Abs (arg, xs)))
      }
  | ty = kind_and_name xs = let_sugar
      { let (ty_xs, xs) = xs in
        let ty_xs =
          let aux (ty_xs, eff) =
            let ty_xs = ParseTree.Forall (ty, ty_xs) in
            ((get_loc $startpos(ty) $endpos(ty), ty_xs), eff)
          in
          BatOption.map aux ty_xs
        in
        (ty_xs, (get_loc $startpos $endpos, ParseTree.TAbs (ty, xs)))
      }

lambda_sugar:
  | arg = arg Arrow t = term
      { ParseTree.Abs (arg, t) }
  | ty = kind_and_name Arrow t = term
      { ParseTree.TAbs (ty, t) }
  | arg = arg xs = lambda_sugar
      { ParseTree.Abs (arg, (get_loc $startpos(xs) $endpos(xs), xs)) }
  | ty = kind_and_name xs = lambda_sugar
      { ParseTree.TAbs (ty, (get_loc $startpos(xs) $endpos(xs), xs)) }


(********* Module utils *********)

import:
  | Import modul = module_name
      { modul }

module_name:
  | name = upperName
      { Ident.Module.of_list name }


(********* Protected rules ***********)

termProtected: protect(termUnclosed, termClosed) { $1 }

typeExprProtected: protect(typeExprUnclosed, typeExprClosed) { $1 }

kindProtected: protect(kindUnclosed, kindClosed) { snd $1 }


(********* Functions ***********)

protect(Unclosed, Closed):
  | LParen x = Unclosed RParen { (get_loc $startpos $endpos, x) }
  | x = Closed { (get_loc $startpos $endpos, x) }

entry(body):
  | imports = list(import) body = body_list(body)
      { (imports, body) }

body_list(body):
  | EOF
      { [] }
  | x = body xs = body_list(body)
      { (get_loc $startpos $endpos(x), x) :: xs }
