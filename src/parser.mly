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
%token Star Phi
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
  | Let r = is_rec name = lowerName x = args(let_aux)
      { (Ident.Name.of_list [name], r, x) }

let_aux:
  | ty = ty_opt Equal t = term
      { (ty, t) }

%inline is_rec:
  | { ParseTree.NonRec }
  | Rec { ParseTree.Rec }

%inline ty_opt:
  | { None }
  | Colon LBracket eff = eff RBracket ty = typeExpr
      { Some (ty, Some eff) }
  | Colon ty = typeExpr
      { Some (ty, None) }

datatype:
  | Type name = UpperName k = kindopt Equal option(Pipe) variants = separated_nonempty_list(Pipe, variant)
      { (Ident.Type.of_list [name], k, variants) }

typeAlias:
  | Type Alias name = UpperName Equal ty = typeExpr
      { (Ident.Type.of_list [name], ty) }

lambda_aux:
  | Arrow t = term
      { t }

termStrictlyUnclosed:
  | Lambda args = nonempty_args(lambda_aux)
      { ParseTree.Abs args }
  | x = let_case In xs = term
      { ParseTree.Let (x, xs) }
  | x = termProtectedPermissive Semicolon y = term
      { ParseTree.Seq (x, y) }

termNonStrictlyUnclosed:
  | x = app
      { x }
  | Fail LBracket ty = typeExpr RBracket exn = effectValue
      { ParseTree.Fail (ty, exn) }

termUnclosed:
  | x = termStrictlyUnclosed { x }
  | x = termNonStrictlyUnclosed { x }

termClosed:
  | name = name
      { ParseTree.Val (Ident.Name.of_list name) }
  | Match t = term With option(Pipe) p = separated_nonempty_list(Pipe, pattern) End
      { ParseTree.PatternMatching (t, p) }
  | Try t = term With option(Pipe) p = separated_nonempty_list(Pipe, exn_pattern) End
      { ParseTree.Try (t, p) }
  | LParen x = term RParen
      { snd x }

term:
  | x = termUnclosed { (get_loc $startpos $endpos, x) }
  | x = termClosed { (get_loc $startpos $endpos, x) }

termProtectedPermissive:
  | x = termNonStrictlyUnclosed { (get_loc $startpos $endpos, x) }
  | x = termClosed { (get_loc $startpos $endpos, x) }

app:
  | f = termProtected LBracket ty = typeExpr RBracket
      { ParseTree.TApp (f, ty) }
  | f = app LBracket ty = typeExpr RBracket
      { ParseTree.TApp ((get_loc $startpos(f) $endpos(f), f), ty) }
  | f = termProtected LBracket LBracket eff = eff RBracket RBracket
      { ParseTree.EApp (f, eff) }
  | f = app LBracket LBracket eff = eff RBracket RBracket
      { ParseTree.EApp ((get_loc $startpos(f) $endpos(f), f), eff) }
  | f = termProtected x = termProtected
      { ParseTree.App (f, x) }
  | f = app x = termProtected
      { ParseTree.App ((get_loc $startpos(f) $endpos(f), f), x) }

arg:
  | LParen name = lowerName Colon ty = typeExpr RParen
      { ParseTree.VArg (Ident.Name.of_list [name], ty) }
  | ty = kind_and_name
      { ParseTree.TArg ty }
  | LParen name = UpperName Colon Phi RParen
      { ParseTree.EArg (Ident.Eff.of_list [name]) }
  | LParen RParen
      { ParseTree.Unit }

args_aux(rest):
  | rest = rest
      { ([], rest) }
  | x = arg xs = args_aux(rest)
      { let (xs, rest) = xs in
        ((get_loc $startpos $endpos, x) :: xs, rest)
      }

args(rest):
  | rest = rest
      { ([], rest) }
  | x = arg xs = args_aux(rest)
      { let (xs, rest) = xs in
        ((get_loc $startpos $endpos, x) :: xs, rest)
      }

nonempty_args(rest):
  | x = arg xs = args_aux(rest)
      { let (xs, rest) = xs in
        ((get_loc $startpos $endpos, x) :: xs, rest)
      }

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

%inline lowerName:
  | name = LowerName
      { name }
  | Underscore
      { "_" }

typeExprUnclosed:
  | param = typeExprProtected Arrow ret = typeExpr
      { ParseTree.Fun (param, None, ret) }
  | param = typeExprProtected LArrowEff eff = eff RArrowEff ret = typeExpr
      { ParseTree.Fun (param, Some eff, ret) }
  | Forall x = nonempty_list(kind_and_name_eff) Comma ret = typeExpr
      { ParseTree.Forall (x, ret) }
  | Lambda x = nonempty_list(kind_and_name) Comma ret = typeExpr
      { ParseTree.AbsOnTy (x, ret) }
  | x = tyApp
      { x }

typeExprClosed:
  | name = upperName
      { ParseTree.Ty (Ident.Type.of_list name) }
  | LParen x = typeExpr RParen
      { snd x }

typeExpr:
  | x = typeExprUnclosed { (get_loc $startpos $endpos, x) }
  | x = typeExprClosed { (get_loc $startpos $endpos, x) }

tyApp:
  | f = typeExprProtected x = typeExprProtected
      { ParseTree.AppOnTy (f, x) }
  | f = tyApp x = typeExprProtected
      { ParseTree.AppOnTy ((get_loc $startpos(f) $endpos(f), f), x) }

kindUnclosed:
  | k1 = kindClosed Arrow k2 = kind
      { Kinds.KFun (k1, k2) }

kindClosed:
  | Star
      { Kinds.Star }
  | LParen x = kind RParen
      { x }

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
  | name = lowerName { Ident.Name.of_list [name] }

variant:
  | name = UpperName Colon ty = typeExpr
      { ParseTree.Variant
          (get_loc $startpos $endpos, Ident.Name.of_list [name], ty)
      }

kindopt:
  | { None }
  | Colon k = kind { Some k }

kind_and_name:
  | name = UpperName
      { (Ident.Type.of_list [name], None) }
  | LParen name = UpperName Colon k = kind RParen
      { (Ident.Type.of_list [name], Some k) }

kind_and_name_eff:
  | k = kind_and_name
      { ParseTree.Typ k }
  | LParen name = UpperName Colon Phi RParen
      { ParseTree.Eff (Ident.Eff.of_list [name]) }

pattern:
  | p = pat Arrow t = term
      { (p, t) }

pat:
  | name = lowerName
      { ParseTree.Any (Ident.Name.of_list [name]) }
  | name = upperName args = list(pat_arg)
      { ParseTree.TyConstr
          (get_loc $startpos $endpos, Ident.Name.of_list name, args)
      }

pat_arg:
  | name = lowerName
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
      { ParseTree.IVal (Ident.Name.of_list [name], ty) }
  | Type name = UpperName k = kindopt
      { ParseTree.IAbstractType (Ident.Type.of_list [name], k) }
  | datatype = datatype
      { ParseTree.IDatatype datatype }
  | typeAlias = typeAlias
      { ParseTree.ITypeAlias typeAlias }
  | Exception name = UpperName args = exceptionArgs
      { ParseTree.IException (Ident.Exn.of_list [name], args) }


(********* Module utils *********)

import:
  | Import modul = module_name
      { modul }

module_name:
  | name = upperName
      { Ident.Module.of_list name }


(********* Protected rules ***********)

termProtected: x = termClosed { (get_loc $startpos $endpos, x) }

typeExprProtected: x = typeExprClosed { (get_loc $startpos $endpos, x) }


(********* Functions ***********)

entry(body):
  | imports = list(import) body = body_list(body)
      { (imports, body) }

body_list(body):
  | EOF
      { [] }
  | x = body xs = body_list(body)
      { (get_loc $startpos $endpos(x), x) :: xs }
