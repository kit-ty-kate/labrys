(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open Utils.PPrint
open DesugaredTree

let dump_name name = str (Ident.Name.to_string name)
let dump_ty_name name = str (Ident.Type.to_string name)
let dump_var_name name = str (Ident.Constr.to_string name)
let dump_tyclass_name name = str (Ident.TyClass.to_string name)
let dump_inst_name name = str (Ident.Instance.to_string name)

let rec dump_kind = function
  | KStar -> str "*"
  | KEff -> str "φ"
  | KExn -> str "^"
  | KFun (x, y) -> dump_kind x ^^^ str "->" ^/^ dump_kind y

let dump_forall_arg (name, k) =
  parens (dump_ty_name name ^^^ colon ^^^ dump_kind k)

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
  let name = dump_tyclass_name name in
  let ty_args = dump_ty_args ty_args in
  let args = dump_tys args in
  name ^^^ braces ty_args ^^^ args

and dump_ty = function
  | (_, Fun (param, eff, res)) ->
      let param = dump_ty param in
      let eff = dump_fun_eff eff in
      let res = dump_ty res in
      parens (param ^^^ eff ^/^ res)
  | (_, Ty name) -> dump_ty_name name
  | (_, Eff eff) -> brackets (dump_eff eff)
  | (_, Sum sum) -> brackets (caret ^^^ dump_sum sum ^^^ caret)
  | (_, Forall (arg, res)) ->
      let arg = dump_forall_arg arg in
      let res = dump_ty res in
      parens (str "forall" ^^^ arg ^^ comma ^//^ res)
  | (_, TyClass (tyclass, eff, res)) ->
      let tyclass = dump_tyclass tyclass in
      let eff = dump_tyclass_eff eff in
      let res = dump_ty res in
      parens (braces tyclass ^^^ eff ^/^ res)
  | (_, AbsOnTy (arg, res)) ->
      let arg = dump_forall_arg arg in
      let res = dump_ty res in
      parens (str "λ" ^^^ arg ^^^ str "->" ^//^ res)
  | (_, AppOnTy (f, x)) ->
      let f = dump_ty f in
      let x = dump_ty x in
      parens (f ^^^ brackets x)

let dump_annot = function
  | (ty, None) -> dump_ty ty
  | (ty, Some eff) -> dump_ty eff ^^^ sharp ^^^ dump_ty ty

let dump_rec = function
  | true -> str "rec"
  | false -> empty

let rec dump_pattern = function
  | TyConstr (_, name, []) -> dump_var_name name
  | TyConstr (_, name, args) -> parens (dump_var_name name ^^^ dump_patterns args)
  | Any name -> dump_name name
  | Or (p1, p2) -> parens (dump_pattern p1) ^^^ bar ^^^ dump_pattern p2
  | As (p, name) -> parens (dump_pattern p) ^^^ str "as" ^^^ dump_name name

and dump_patterns l = separate_map space dump_pattern l

let dump_try_pattern _ = assert false

let dump_tyclass_instance (name, tys) =
  dump_tyclass_name name ^^^ separate_map space dump_ty tys

let dump_tyclass_app_arg = function
  | TyClassVariable name -> dump_inst_name name
  | TyClassInstance instance -> dump_tyclass_instance instance

let rec dump_let ~is_rec (name, t) =
  str "let" ^^^ dump_rec is_rec ^^^ dump_name name ^^^ equals ^//^ dump_t t

and dump_pattern_matching : type a. _ -> (a -> _) -> (_ * (a * _) list) -> _ =
  fun name f (t, cases) ->
    str name ^^^ dump_t t ^^^ str "with" ^/^ dump_cases f cases ^/^ str "end"

and dump_t = function
  | (_, Abs ((name, ty), t)) ->
      parens (str "λ" ^^^ parens (dump_name name ^^^ colon ^^^ dump_ty ty) ^^^
              str "->" ^//^ dump_t t)
  | (_, TAbs ((name, k), t)) ->
      parens (str "λ" ^^^ parens (dump_ty_name name ^^^ colon ^^^ dump_kind k) ^^^
              str "->" ^//^ dump_t t)
  | (_, CAbs ((name, tyclass), t)) ->
      parens (str "λ" ^^^ parens (dump_inst_name name ^^^ colon ^^^ dump_tyclass tyclass) ^^^
              str "->" ^//^ dump_t t)
  | (_, App (f, x)) ->
      parens (dump_t f ^//^ dump_t x)
  | (_, TApp (f, ty)) ->
      parens (dump_t f ^//^ brackets (dump_ty ty))
  | (_, CApp (f, x)) ->
      parens (dump_t f ^//^ qmark ^^ brackets (dump_tyclass_app_arg x))
  | (_, Val name) ->
      dump_name name
  | (_, Var name) ->
      dump_var_name name
  | (_, PatternMatching t) ->
      dump_pattern_matching "match" dump_pattern t
  | (_, Let (name, x, xs)) ->
      parens (group (dump_let ~is_rec:false (name, x) ^/^ str "in") ^/^ dump_t xs)
  | (_, LetRec (name, x, xs)) ->
      parens (group (dump_let ~is_rec:true (name, x) ^/^ str "in") ^/^ dump_t xs)
  | (_, Fail (ty, t)) ->
      parens (str "fail" ^^^ brackets (dump_ty ty) ^^^ dump_t t)
  | (_, Try t) ->
      dump_pattern_matching "try" dump_try_pattern t
  | (_, Annot (t, ty)) ->
      parens (dump_t t ^^^ colon ^^^ dump_annot ty)
  | (_, Const (Int n)) ->
      OCaml.int n
  | (_, Const (Float n)) ->
      OCaml.float n
  | (_, Const (Char c)) ->
      squotes (str (Utils.string_of_uchar c))
  | (_, Const (String s)) ->
      dquotes (str s)

and dump_cases : type a. (a -> _) -> (a * _) list -> _ = fun f cases ->
  let aux (pattern, t) =
    bar ^^^ f pattern ^^^ str "->" ^^ jump 4 1 (dump_t t)
  in
  separate_map hardline aux cases

let dump_variants variants =
  let aux (name, ty) =
    bar ^^^ dump_var_name name ^^^ colon ^^^ dump_ty ty
  in
  separate_map hardline aux variants

let dump_sig (name, ty) =
  str "let" ^^^ dump_name name ^^^ colon ^^^ dump_ty ty

let dump_sigs sigs = separate_map (hardline ^^ hardline) dump_sig sigs

let dump_cname cname = dquotes (str cname)

let dump_instance_name = function
  | None -> empty
  | Some name -> brackets (dump_inst_name name)

let dump_lets values =
  separate_map (hardline ^^ hardline) (dump_let ~is_rec:false) values

let dump_class name params =
  dump_tyclass_name name ^^^ dump_forall_args params

let dump_instance name tyclass =
  dump_instance_name name ^^^ dump_tyclass_instance tyclass

let dump_top = function
  | Value x ->
      dump_let ~is_rec:false x
  | Type (name, ty) ->
      str "type alias" ^^^ dump_ty_name name ^^^ equals ^//^ dump_ty ty
  | Foreign (cname, name, ty) ->
      str "foreign" ^^^ dump_cname cname ^^^ dump_name name ^^^ colon ^//^
      dump_ty ty
  | Datatype (name, k, variants) ->
      str "type" ^^^ dump_ty_name name ^^^ colon ^^^ dump_kind k ^^^ equals ^//^
      dump_variants variants
  | Exception (name, ty) ->
      str "exception" ^^^ dump_var_name name ^^^ colon ^^^ dump_ty ty
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
