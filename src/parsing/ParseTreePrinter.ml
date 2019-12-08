(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

open Utils.PPrint
open ParseTree

let dump_name = function
  | (_, `Underscore) -> str "_"
  | (_, `NewLowerName name) | (_, `NewUpperName name) -> str name
  | (_, `LowerName name) | (_, `UpperName name) -> str (String.concat "." name)

let rec dump_kind = function
  | KStar -> str "*"
  | KEff -> str "φ"
  | KExn -> str "^"
  | KFun (x, y) -> parens (dump_kind x ^^^ str "->" ^/^ dump_kind y)

let dump_kind_opt = function
  | Some k -> dump_kind k
  | None -> empty

let dump_module = function
  | Source name -> dump_name name
  | Library name -> dump_name name

let dump_forall_arg = function
  | (name, Some k) ->
      parens (dump_name name ^^^ colon ^^^ dump_kind k)
  | (name, None) ->
      dump_name name

let dump_forall_args l = separate_map space dump_forall_arg l
let dump_ty_args l = separate_map (comma ^^ space) dump_forall_arg l

let rec dump_tys tys = separate_map space dump_ty tys
and dump_eff (_, x) = separate_map (comma ^^ space) dump_ty x
and dump_sum x = separate_map (space ^^ bar ^^ space) dump_ty x

and dump_fun_eff = function
  | None -> str "->"
  | Some eff -> str "-[" ^^ dump_eff eff ^^ str "]->"

and dump_tyclass_eff = function
  | None -> str "=>"
  | Some eff -> str "=[" ^^ dump_eff eff ^^ str "]=>"

and dump_tyclass (name, ty_args, args) =
  let name = dump_name name in
  let ty_args = dump_ty_args ty_args in
  let args = dump_tys args in
  name ^^^ braces ty_args ^^^ args

and dump_ty = function
  | (_, Fun (param, eff, res)) ->
      let param = dump_ty param in
      let eff = dump_fun_eff eff in
      let res = dump_ty res in
      parens (param ^^^ eff ^/^ res)
  | (_, Ty name) -> dump_name name
  | (_, TyVar name) -> dump_name name
  | (_, Eff eff) -> brackets (dump_eff eff)
  | (_, Sum sum) -> brackets (caret ^^^ dump_sum sum ^^^ caret)
  | (_, Forall (names, res)) ->
      let names = dump_forall_args names in
      let res = dump_ty res in
      parens (str "forall" ^^^ names ^^ comma ^//^ res)
  | (_, TyClass (tyclass, eff, res)) ->
      let tyclass = dump_tyclass tyclass in
      let eff = dump_tyclass_eff eff in
      let res = dump_ty res in
      parens (braces tyclass ^^^ eff ^/^ res)
  | (_, AbsOnTy (names, res)) ->
      let names = dump_forall_args names in
      let res = dump_ty res in
      parens (str "λ" ^^^ names ^^^ str "->" ^//^ res)
  | (_, AppOnTy (f, x)) ->
      let f = dump_ty f in
      let x = dump_ty x in
      parens (f ^^^ brackets x)

let dump_annot = function
  | (ty, None) -> dump_ty ty
  | (ty, Some eff) -> dump_ty eff ^^^ sharp ^^^ dump_ty ty

let dump_annot_opt = function
  | None -> empty
  | Some x -> colon ^^^ dump_annot x

let dump_arg = function
  | (_, VArg (name, ty)) ->
      parens (dump_name name ^^^ colon ^^^ dump_ty ty)
  | (_, TArg v) ->
      dump_forall_arg v
  | (_, Unit) ->
      parens empty
  | (_, TyClassArg (name, tyclass)) ->
      parens (dump_name name ^^^ colon ^^^ dump_tyclass tyclass)

let dump_args l ty = separate_map space dump_arg l ^^^ dump_annot_opt ty

let dump_rec = function
  | Rec -> str "rec"
  | NonRec -> empty

let rec dump_pattern = function
  | TyConstr (_, name, []) -> dump_name name
  | TyConstr (_, name, args) -> parens (dump_name name ^^^ dump_patterns args)
  | Any name -> dump_name name
  | Or (p1, p2) -> parens (dump_pattern p1) ^^^ bar ^^^ dump_pattern p2
  | As (p, name) -> parens (dump_pattern p) ^^^ str "as" ^^^ dump_name name

and dump_patterns l = separate_map space dump_pattern l

let dump_tyclass_instance (name, tys) =
  dump_name name ^^^ separate_map space dump_ty tys

let dump_tyclass_app_arg = function
  | TyClassVariable name -> dump_name name
  | TyClassInstance instance -> dump_tyclass_instance instance

let rec dump_let (name, is_rec, (args, (ty, t))) =
  str "let" ^^^ dump_rec is_rec ^^^ dump_name name ^^^ dump_args args ty ^^^
  equals ^//^ dump_t t

and dump_pattern_matching name (t, cases) =
  str name ^^^ dump_t t ^^^ str "with" ^/^ dump_cases cases ^/^ str "end"

and dump_t = function
  | (_, Abs (args, (ty, t))) ->
      parens (str "λ" ^^^ dump_args args ty ^^^
              str "->" ^//^ dump_t t)
  | (_, App (f, x)) ->
      parens (dump_t f ^//^ dump_t x)
  | (_, TApp (f, ty)) ->
      parens (dump_t f ^//^ brackets (dump_ty ty))
  | (_, TyClassApp (f, x)) ->
      parens (dump_t f ^//^ qmark ^^ brackets (dump_tyclass_app_arg x))
  | (_, LowerVal name) ->
      dump_name name
  | (_, UpperVal name) ->
      dump_name name
  | (_, PatternMatching t) ->
      dump_pattern_matching "match" t
  | (_, Let (x, xs)) ->
      parens (group (dump_let x ^/^ str "in") ^/^ dump_t xs)
  | (_, Fail (ty, t)) ->
      parens (str "fail" ^^^ brackets (dump_ty ty) ^^^ dump_t t)
  | (_, Try t) ->
      dump_pattern_matching "try" t
  | (_, Seq (x, y)) ->
      parens (dump_t x ^^ semi ^/^ dump_t y)
  | (_, Annot (t, ty)) ->
      parens (dump_t t ^^^ colon ^^^ dump_annot ty)
  | (_, Const (Int n)) ->
      str n
  | (_, Const (Float n)) ->
      str n
  | (_, Const (Char c)) ->
      squotes (str (String.of_list c))
  | (_, Const (Bytes s)) ->
      dquotes (str (String.of_list s))

and dump_cases cases =
  let aux (pattern, t) =
    bar ^^^ dump_pattern pattern ^^^ str "->" ^^ jump 4 1 (dump_t t)
  in
  separate_map hardline aux cases

let dump_variants variants =
  let aux (name, tys) =
    bar ^^^ dump_name name ^^^ separate_map space dump_ty tys
  in
  separate_map hardline aux variants

let dump_sig (name, ty) =
  str "let" ^^^ dump_name name ^^^ colon ^^^ dump_ty ty

let dump_sigs sigs = separate_map (hardline ^^ hardline) dump_sig sigs

let dump_cname cname = dquotes (string (String.of_list cname))

let dump_instance_name = function
  | None -> empty
  | Some name -> brackets (dump_name name)

let dump_lets values =
  separate_map (hardline ^^ hardline) dump_let values

let dump_class name params =
  dump_name name ^^^ dump_forall_args params

let dump_instance name tyclass =
  dump_instance_name name ^^^ dump_tyclass_instance tyclass

let dump_top = function
  | Value x ->
      dump_let x
  | Type (name, ty) ->
      str "type alias" ^^^ dump_name name ^^^ equals ^//^ dump_ty ty
  | Foreign (cname, name, ty) ->
      str "foreign" ^^^ dump_cname cname ^^^ dump_name name ^^^ colon ^//^
      dump_ty ty
  | AbstractType (name, k) ->
      str "type" ^^^ dump_name name ^^^ dump_kind_opt k
  | Datatype (name, args, variants) ->
      str "type" ^^^ dump_name name ^^^ dump_forall_args args ^^^ equals ^//^
      dump_variants variants
  | Exception (name, args) ->
      str "exception" ^^^ dump_name name ^^^ dump_tys args
  | Open modul ->
      str "open" ^^^ dump_module modul
  | Class (name, params, sigs) ->
      group (
        (str "class" ^^^ dump_class name params ^^^ equals ^//^
         dump_sigs sigs) ^/^
        str "end")
  | Instance (tyclass, name, values) ->
      group (
        (str "instance" ^^^ dump_instance name tyclass ^^^ equals ^//^
         dump_lets values) ^/^
        str "end")

let dump l =
  separate_map (hardline ^^ hardline) dump_top l
