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

open BatteriesExceptionless
open Monomorphic.None

let fmt = Printf.sprintf
let (^^) = PPrint.(^^)

let dump_name = Gamma.Name.to_string
let dump_t_name = Gamma.Type.to_string
let dump_k = Kinds.to_string

let rec dump_top f doc = function
  | [] -> doc
  | [x] -> doc ^^ f x
  | x::xs -> dump_top f (doc ^^ f x ^^ PPrint.hardline ^^ PPrint.hardline) xs

module ParseTree = struct
  open ParseTree

  let rec dump_ty = function
    | Fun (param, res) -> fmt "(%s -> %s)" (dump_ty param) (dump_ty res)
    | Ty name -> dump_t_name name
    | Forall ((name, k), res) ->
        fmt "(forall %s : %s, %s)" (dump_t_name name) (dump_k k) (dump_ty res)
    | AbsOnTy ((name, k), res) ->
        fmt "(λ (%s : %s) -> %s)" (dump_t_name name) (dump_k k) (dump_ty res)
    | AppOnTy (f, x) ->
        fmt "(%s [%s])" (dump_ty f) (dump_ty x)

  let rec dump_pattern = function
    | TyConstr (name, []) ->
        dump_name name
    | TyConstr (name, args) ->
        fmt
          "(%s %s)"
          (dump_name name)
          (String.concat " " (List.map dump_pattern_arg args))
    | Any name ->
        dump_name name

  and dump_pattern_arg = function
    | PVal p -> dump_pattern p
    | PTy ty -> fmt "[%s]" (dump_ty ty)

  let rec dump_t = function
    | Abs (_, (name, ty), t) ->
        PPrint.group
          (PPrint.lparen
           ^^ PPrint.string (fmt "λ (%s : %s) ->" (dump_name name) (dump_ty ty))
           ^^ PPrint.nest 2 (PPrint.break 1 ^^ dump_t t)
           ^^ PPrint.rparen
          )
    | TAbs (_, (name, k), t) ->
        PPrint.group
          (PPrint.lparen
           ^^ PPrint.string (fmt "λ (%s : %s) ->" (dump_t_name name) (dump_k k))
           ^^ PPrint.nest 2 (PPrint.break 1 ^^ dump_t t)
           ^^ PPrint.rparen
          )
    | App (_, f, x) ->
        PPrint.group
          (PPrint.lparen
           ^^ dump_t f
           ^^ PPrint.nest 2 (PPrint.break 1 ^^ dump_t x)
           ^^ PPrint.rparen
          )
    | TApp (_, f, ty) ->
        PPrint.group
          (PPrint.lparen
           ^^ dump_t f
           ^^ PPrint.blank 1
           ^^ PPrint.string (fmt "[%s]" (dump_ty ty))
           ^^ PPrint.rparen
          )
    | Val (_, name) ->
        PPrint.string (dump_name name)
    | PatternMatching (_, t, cases) ->
        PPrint.group
          (PPrint.string "match"
           ^^ PPrint.break 1
           ^^ dump_t t
           ^^ PPrint.break 1
           ^^ PPrint.string "with"
          )
        ^^ dump_cases cases
        ^^ PPrint.break 1
        ^^ PPrint.string "end"
    | Let (name, t, xs) ->
        PPrint.group
          (PPrint.lparen
           ^^ PPrint.string (fmt "let %s =" (dump_name name))
           ^^ PPrint.nest 2 (PPrint.break 1 ^^ dump_t t)
           ^^ PPrint.break 1
           ^^ PPrint.string "in"
           ^^ PPrint.break 1
           ^^ dump_t xs
           ^^ PPrint.rparen
          )
    | LetRec (_, name, ty, t, xs) ->
        PPrint.group
          (PPrint.lparen
           ^^ PPrint.string (fmt "let %s : %s =" (dump_name name) (dump_ty ty))
           ^^ PPrint.nest 2 (PPrint.break 1 ^^ dump_t t)
           ^^ PPrint.break 1
           ^^ PPrint.string "in"
           ^^ PPrint.break 1
           ^^ dump_t xs
           ^^ PPrint.rparen
          )

  and dump_cases cases =
    let aux doc ((_, pattern), (_, t)) =
      doc
      ^^ PPrint.break 1
      ^^ PPrint.group
           (PPrint.string (fmt "| %s ->" (dump_pattern pattern))
            ^^ PPrint.nest 4 (PPrint.break 1 ^^ dump_t t)
           )
    in
    List.fold_left aux PPrint.empty cases

  let dump_variants (Variant (_, name, ty)) =
    PPrint.string (fmt "| %s : %s" (dump_name name) (dump_ty ty))

  let dump_variants variants =
    let aux doc x = doc ^^ PPrint.break 1 ^^ dump_variants x in
    List.fold_left aux PPrint.empty variants

  let dump = function
    | Value (name, t) ->
        PPrint.group
          (PPrint.string (fmt "let %s =" (dump_name name))
           ^^ (PPrint.nest 2 (PPrint.break 1 ^^ dump_t t))
          )
    | RecValue (_, name, ty, t) ->
        PPrint.group
          (PPrint.string (fmt "let %s : %s =" (dump_name name) (dump_ty ty))
           ^^ PPrint.nest 2 (PPrint.break 1 ^^ dump_t t)
          )
    | Type (_, name, ty) ->
        PPrint.string (fmt "type alias %s = %s" (dump_t_name name) (dump_ty ty))
    | Binding (_, name, ty, content) ->
        PPrint.string (fmt "let %s : %s = begin" (dump_name name) (dump_ty ty))
        ^^ PPrint.string content
        ^^ PPrint.string "end"
    | Datatype (_, name, k, variants) ->
        PPrint.string (fmt "type %s : %s =" (dump_t_name name) (dump_k k))
        ^^ PPrint.nest 2 (dump_variants variants)

  let dump top =
    let doc = dump_top dump PPrint.empty top in
    let buf = Buffer.create 1024 in
    PPrint.ToBuffer.pretty 0.9 80 buf doc;
    Buffer.contents buf
end
