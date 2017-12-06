(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open Utils.PPrint
open UntypedTree

let dump_name name = str (Ident.Name.to_string name)

let dump_constr_rep = function
  | Index idx -> OCaml.int idx
  | Exn name -> dump_name name

let rec dump_tree = function
  | Switch (cases, def) ->
      str "Switch" ^^^ parens (dump_switch cases ^^ comma ^^^ dump_tree_opt def)
  | Swap (idx, t) ->
      str "Swap" ^^^ parens (OCaml.int idx ^^ comma ^^^ dump_tree t)
  | Alias (name, t) ->
      str "Alias" ^^^ parens (dump_name name ^^ comma ^^^ dump_tree t)
  | Jump branch ->
      str "Jump" ^^^ OCaml.int branch
and dump_tree_opt = function
  | None -> str "None"
  | Some t -> dump_tree t
and dump_switch cases =
  brackets (separate_map (semi ^^ space) dump_switch_case cases)
and dump_switch_case (constr, len, t) =
  parens (dump_constr_rep constr ^^ comma ^^^ OCaml.int len ^^ comma ^^^ dump_tree t)

let dump_try_pattern (exn, args) =
  dump_name exn ^^^ separate_map space dump_name args

let dump_list l =
  brackets (separate (comma ^^ space) l)

let rec dump_rec_let (name, t) =
  str "let" ^^^ str "rec" ^^^ dump_name name ^^^ equals ^//^ dump_t t

and dump_let (name, t) =
  str "let" ^^^ dump_name name ^^^ equals ^//^ dump_t t

and dump_pattern_matching : type a. _ -> (a -> _) -> (_ * (a * _) list) -> _ =
  fun name f (t, cases) ->
    str name ^^^ dump_t t ^^^ str "with" ^/^ dump_cases f cases ^/^ str "end"

and dump_branch t =
  bar ^^^ dump_t t

and dump_t = function
  | Abs (name, t) ->
      parens (str "λ" ^^^ dump_name name ^^^ str "->" ^//^ dump_t t)
  | App (f, x) ->
      parens (dump_t f ^//^ dump_t x)
  | Val name ->
      dump_name name
  | Var (rep, len) ->
      dump_list [dump_constr_rep rep; OCaml.int len]
  | PatternMatching (t, vars, branches, tree) ->
      str "match" ^^^ dump_t t ^^^ str "with" ^/^
      braces (separate_map space dump_name (Ident.Name.Set.to_list vars)) ^^^ str "in" ^/^
      separate_map hardline dump_branch branches ^/^
      str "from" ^^^ dump_tree tree ^/^
      str "end"
  | Let (name, x, xs) ->
      parens (group (dump_let (name, x) ^/^ str "in") ^/^ dump_t xs)
  | LetRec (name, x, xs) ->
      parens (group (dump_rec_let (name, x) ^/^ str "in") ^/^ dump_t xs)
  | Fail t ->
      parens (str "fail" ^^^ dump_t t)
  | Try t ->
      dump_pattern_matching "try" dump_try_pattern t
  | RecordGet (t, n) ->
      dump_t t ^^ dot ^^ OCaml.int n
  | RecordCreate fields ->
      dump_list (List.map dump_t fields)
  | Const (`Int n) ->
      OCaml.int n
  | Const (`Float n) ->
      OCaml.float n
  | Const (`Char c) ->
      squotes (str (Utils.string_of_uchar c))
  | Const (`String s) ->
      dquotes (str s)

and dump_cases : type a. (a -> _) -> (a * _) list -> _ = fun f cases ->
  let aux (pattern, t) =
    bar ^^^ f pattern ^^^ str "->" ^^ jump 4 1 (dump_t t)
  in
  separate_map hardline aux cases

let dump_cname cname = dquotes (str cname)

let dump_lets values =
  separate_map (hardline ^^ hardline) dump_let values

let dump_tag_ty = function
  | `Int () -> str "Int"
  | `Float () -> str "Float"
  | `Char () -> str "Char"
  | `String () -> str "String"
  | `Custom -> str "Custom"
  | `Void -> str "Void"

let dump_args_ty args =
  dump_list (List.map dump_tag_ty args)

let dump_top = function
  | Value x ->
      dump_let x
  | Foreign (cname, name, (ret, args)) ->
      str "foreign" ^^^ dump_cname cname ^^^ dump_name name ^^^ colon ^//^
      dump_args_ty args ^^^ str "->" ^^^ dump_tag_ty ret
  | Exception name ->
      str "exception" ^^^ dump_name name
  | Instance (name, values) ->
      group (
        (str "instance" ^^^ dump_name name ^^^ equals ^//^
         dump_lets values) ^/^
        str "end")

let dump l =
  separate_map (hardline ^^ hardline) dump_top l
