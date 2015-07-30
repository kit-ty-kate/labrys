(*
Copyright (c) 2013-2015 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

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

%parameter < Filename : sig val get : string end >

%{
  let loc startpos endpos =
    let get_pos pos =
      { Location.pos_lnum = pos.Lexing.pos_lnum
      ; pos_cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol
      }
    in
    { Location.loc_start = get_pos startpos
    ; loc_end = get_pos endpos
    ; filename = Filename.get
    }
%}

%token Import Library
%token Open
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
%token Star Eff
%token Fail
%token Try
%token Exception
%token Underscore
%token Semicolon
%token <string> LowerName
%token <string> UpperName
%token <string> Binding
%token LParen RParen
%token LBracket RBracket
%token EOF

%start main
%type <(ParseTree.imports * ParseTree.top list)> main

%start mainInterface
%type <(ParseTree.imports * ParseTree.interface list)> mainInterface

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
  | Let name = newLowerName Colon ty = typeExpr Equal binding = Binding
      { ParseTree.Binding (name, ty, binding) }
  | datatype = datatype
      { ParseTree.Datatype datatype }
  | Exception name = newUpperName args = exceptionArgs
      { ParseTree.Exception (name, args) }
  | Open modul = upperName
      { ParseTree.Open modul }

exceptionArgs:
  | x = typeExprClosed xs = exceptionArgs
      { x :: xs }
  | { [] }

let_case:
  | Let r = is_rec name = newLowerName x = args(let_aux)
      { (name, r, x) }

let_aux:
  | ty = ty_opt(typeExpr) Equal t = term
      { (ty, t) }

%inline is_rec:
  | { ParseTree.NonRec }
  | Rec { ParseTree.Rec }

%inline ty_annot(ty):
  | Colon LBracket LBracket eff = eff RBracket RBracket ty = ty
      { (ty, Some eff) }
  | Colon ty = ty
      { (ty, None) }

%inline ty_opt(ty):
  | { None }
  | ty = ty_annot(ty)
      { Some ty }

datatype:
  | Type name = newUpperName args = list(kind_and_name) Equal option(Pipe) variants = separated_nonempty_list(Pipe, variant)
      { (name, args, variants) }

typeAlias:
  | Type Alias name = newUpperName Equal ty = typeExpr
      { (name, ty) }

lambda_aux:
  | ty = ty_opt(typeExprProtectedPermissive) Arrow t = term
      { (ty, t) }

termStrictlyUnclosed:
  | Lambda args = nonempty_args(lambda_aux)
      { (loc $startpos $endpos, ParseTree.Abs args) }
  | x = let_case In xs = term
      { (loc $startpos $endpos, ParseTree.Let (x, xs)) }
  | x = termProtectedPermissive Semicolon y = term
      { (loc $startpos $endpos, ParseTree.Seq (x, y)) }
  | t = termProtectedPermissive ty = ty_annot(typeExpr)
      { (loc $startpos $endpos, ParseTree.Annot (t, ty)) }

termNonStrictlyUnclosed:
  | x = app
      { x }
  | Fail LBracket ty = typeExpr RBracket exn = exceptionValue
      { (loc $startpos $endpos, ParseTree.Fail (ty, exn)) }

termUnclosed:
  | x = termStrictlyUnclosed { x }
  | x = termNonStrictlyUnclosed { x }

termClosed:
  | name = lowerName
      { (loc $startpos $endpos, ParseTree.LowerVal name) }
  | name = upperName
      { (loc $startpos $endpos, ParseTree.UpperVal name) }
  | Match t = term With option(Pipe) p = separated_nonempty_list(Pipe, pattern) End
      { (loc $startpos $endpos, ParseTree.PatternMatching (t, p)) }
  | Try t = term With option(Pipe) p = separated_nonempty_list(Pipe, exn_pattern) End
      { (loc $startpos $endpos, ParseTree.Try (t, p)) }
  | LParen x = term RParen
      { x }

term:
  | x = termUnclosed { x }
  | x = termClosed { x }

termProtectedPermissive:
  | x = termNonStrictlyUnclosed { x }
  | x = termClosed { x }

app:
  | f = termClosed LBracket ty = typeExpr RBracket
      { (loc $startpos $endpos, ParseTree.TApp (f, ty)) }
  | f = app LBracket ty = typeExpr RBracket
      { (loc $startpos $endpos, ParseTree.TApp (f, ty)) }
  | f = termClosed x = termClosed
      { (loc $startpos $endpos, ParseTree.App (f, x)) }
  | f = app x = termClosed
      { (loc $startpos $endpos, ParseTree.App (f, x)) }

arg:
  | LParen name = newLowerName Colon ty = typeExpr RParen
      { ParseTree.VArg (name, ty) }
  | ty = kind_and_name
      { ParseTree.TArg ty }
  | LParen RParen
      { ParseTree.Unit }

args(rest):
  | rest = rest
      { ([], rest) }
  | x = arg xs = args(rest)
      { let (xs, rest) = xs in
        ((loc $startpos $endpos, x) :: xs, rest)
      }

nonempty_args(rest):
  | x = arg xs = args(rest)
      { let (xs, rest) = xs in
        ((loc $startpos $endpos, x) :: xs, rest)
      }

typeExprStrictlyUnclosed:
  | param = typeExprProtectedPermissive Arrow ret = typeExpr
      { (loc $startpos $endpos, ParseTree.Fun (param, None, ret)) }
  | param = typeExprProtectedPermissive LArrowEff eff = eff RArrowEff ret = typeExpr
      { (loc $startpos $endpos, ParseTree.Fun (param, Some eff, ret)) }
  | Forall x = nonempty_list(forallItems) Comma ret = typeExpr
      { (loc $startpos $endpos, ParseTree.Forall (x, ret)) }
  | Lambda x = nonempty_list(kind_and_name) Comma ret = typeExpr
      { (loc $startpos $endpos, ParseTree.AbsOnTy (x, ret)) }

typeExprNonStrictlyUnclosed:
  | x = tyApp
      { x }

typeExprUnclosed:
  | x = typeExprStrictlyUnclosed { x }
  | x = typeExprNonStrictlyUnclosed { x }

typeExprClosed:
  | name = upperName
      { (loc $startpos $endpos, ParseTree.Ty name) }
  | LBracket eff = eff RBracket
      { (loc $startpos(eff) $endpos(eff), ParseTree.Eff eff) }
  | LParen x = typeExpr RParen
      { x }

typeExpr:
  | x = typeExprUnclosed { x }
  | x = typeExprClosed { x }

typeExprProtectedPermissive:
  | x = typeExprNonStrictlyUnclosed { x }
  | x = typeExprClosed { x }

tyApp:
  | f = typeExprClosed x = typeExprClosed
      { (loc $startpos $endpos, ParseTree.AppOnTy (f, x)) }
  | f = tyApp x = typeExprClosed
      { (loc $startpos $endpos, ParseTree.AppOnTy (f, x)) }

kindUnclosed:
  | k1 = kindClosed Arrow k2 = kind
      { Kinds.KFun (k1, k2) }

kindClosed:
  | Star
      { Kinds.Star }
  | Eff
      { Kinds.Eff }
  | LParen x = kind RParen
      { x }

kind:
  | x = kindUnclosed { x }
  | x = kindClosed { x }

eff: eff = separated_list(Comma, effectName) { (loc $startpos $endpos, eff) }

effectName:
  | name = upperName
      { (name, []) }
  | name = upperName LBracket args = eff_exn RBracket
      { (name, args) }

eff_exn:
  | name = upperName
      { [name] }
  | name = upperName Pipe xs = eff_exn
      { name :: xs }

exceptionValue:
  | LParen name = upperName args = exceptionValueArgs RParen
      { (name, args) }
  | name = upperName
      { (name, []) }

exceptionValueArgs:
  | x = termClosed xs = exceptionValueArgs
      { x :: xs }
  | x = termClosed
      { [x] }

exn_pattern:
  | exn = upperName args = list(newLowerName) Arrow t = term
      { ((exn, args), t) }

variant:
  | name = newUpperName tys = list(typeExprClosed)
      { ParseTree.Variant (name, tys) }

kindopt:
  | { None }
  | Colon k = kind { Some k }

kind_and_name:
  | name = newUpperName
      { (name, None) }
  | LParen name = newUpperName Colon k = kind RParen
      { (name, Some k) }

forallItems:
  | k = kind_and_name
      { ParseTree.Typ k }
  | LParen name = upperName args = nonempty_list(newUpperName) RParen
      { ParseTree.TyClass (name, args) }

pattern:
  | p = pat Arrow t = term
      { (p, t) }

patClosed:
  | name = newLowerName
      { ParseTree.Any name }
  | name = upperName
      { ParseTree.TyConstr (loc $startpos $endpos, name, []) }

patUnclosed:
  | name = upperName args = nonempty_list(patProtected)
      { ParseTree.TyConstr (loc $startpos $endpos, name, args) }

pat:
  | p = patClosed { p }
  | p = patUnclosed { p }

patProtected:
  | p = patClosed { p }
  | LParen p = patUnclosed RParen { p }


(********* Names *********)

lowerName_aux:
  | name = LowerName
      { [name] }
  | m = UpperName Dot xs = lowerName_aux
      { m :: xs }

lowerName: x = lowerName_aux { (loc $startpos $endpos, `LowerName x) }

upperName_aux:
  | name = UpperName
      { [name] }
  | m = UpperName Dot xs = upperName_aux
      { m :: xs }

upperName: x = upperName_aux { (loc $startpos $endpos, `UpperName x) }

%inline newLowerName:
  | name = LowerName
      { (loc $startpos $endpos, `NewLowerName name) }
  | Underscore
      { (loc $startpos $endpos, `Underscore) }

newUpperName:
  | name = UpperName
      { (loc $startpos $endpos, `NewUpperName name) }


(********* Interface *********)

bodyInterface:
  | Let name = newLowerName Colon ty = typeExpr
      { ParseTree.IVal (name, ty) }
  | Type name = newUpperName k = kindopt
      { ParseTree.IAbstractType (name, k) }
  | datatype = datatype
      { ParseTree.IDatatype datatype }
  | typeAlias = typeAlias
      { ParseTree.ITypeAlias typeAlias }
  | Exception name = newUpperName args = exceptionArgs
      { ParseTree.IException (name, args) }
  | Open modul = upperName
      { ParseTree.IOpen modul }


(********* Module utils *********)

import:
  | Import modul = upperName
      { ParseTree.Source modul }
  | Import Library modul = upperName
      { ParseTree.Library modul }


(********* Functions ***********)

entry(body):
  | imports = list(import) body = body_list(body)
      { (imports, body) }

body_list(body):
  | EOF
      { [] }
  | x = body xs = body_list(body)
      { x :: xs }
