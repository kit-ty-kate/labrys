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

type kind = ParseTree.kind =
  | Star
  | KFun of (kind * kind)

type ty =
  | Fun of (ty * ty)
  | Ty of string
  | Forall of (string * kind * ty)
  | AbsOnTy of (string * kind * ty)
  | AppOnTy of (ty * ty)

type t = (ty * kind)

type env = (string * t)

let get_kind = Option.default Star

let rec kind_to_string = function
  | Star -> "*"
  | KFun (p, r) -> kind_to_string p ^ " -> " ^ kind_to_string r

let rec kind_equal x y = match x, y with
  | Star, Star -> true
  | KFun (p1, r1), KFun (p2, r2) -> kind_equal p1 p2 && kind_equal r1 r2
  | Star, KFun _
  | KFun _, Star -> false

let rec to_string = function
  | Fun (Ty x, ret) -> x ^ " -> " ^ to_string ret
  | Fun (x, ret) -> "(" ^ to_string x ^ ") -> " ^ to_string ret
  | Ty x -> x
  | Forall (x, k, t) ->
      fmt "forall %s : %s. %s" x (kind_to_string k) (to_string t)
  | AbsOnTy (name, k, t) ->
      fmt "λ%s : %s. %s" name (kind_to_string k) (to_string t)
  | AppOnTy (Ty f, Ty x) -> fmt "%s %s" f x
  | AppOnTy (Ty f, x) -> fmt "%s (%s)" f (to_string x)
  | AppOnTy (f, Ty x) -> fmt "(%s) %s" (to_string f) x
  | AppOnTy (f, x) -> fmt "(%s) (%s)" (to_string f) (to_string x)

let type_error = fmt "The type '%s' was not found in Γ"

let rec from_parse_tree ~loc gamma = function
  | ParseTree.Fun (x, y) ->
      let (x, _) = from_parse_tree ~loc gamma x in
      let (y, _) = from_parse_tree ~loc gamma y in
      (Fun (x, y), Star)
  | ParseTree.Ty name ->
      let x = List.find (fun x -> String.equal (fst x) name) gamma in
      let x = Option.get_exn x (Error.Exn (loc, type_error name)) in
      snd x
  | ParseTree.Forall (name, k, ret) ->
      let k = get_kind k in
      let (ret, _) = from_parse_tree ~loc ((name, (Ty name, k)) :: gamma) ret in
      (Forall (name, k, ret), Star)
  | ParseTree.AbsOnTy (name, k, ret) ->
      let k = get_kind k in
      let (ret, kret) = from_parse_tree ~loc ((name, (Ty name, k)) :: gamma) ret in
      (AbsOnTy (name, k, ret), KFun (k, kret))
  | ParseTree.AppOnTy (f, x) ->
      let (f, kf) = from_parse_tree ~loc gamma f in
      let (x, kx) = from_parse_tree ~loc gamma x in
      let k =
        match kf with
        | KFun (p, r) when kind_equal p kx -> r
        | KFun _
        | Star ->
            raise
              (Error.Exn
                 (loc,
                  fmt "Kind '%s' doesn't match with '%s'"
                    (kind_to_string kf)
                    (kind_to_string kx)
                 )
              )
      in
      (AppOnTy (f, x), k)

let replace ~from ~ty =
  let rec aux = function
    | Fun (param, ret) -> Fun (aux param, aux ret)
    | Ty x when String.equal x from -> ty
    | Ty x -> Ty x
    | (AbsOnTy (x, _, _) as ty)
    | (Forall (x, _, _) as ty) when String.equal x from -> ty
    | Forall (x, k, t) -> Forall (x, k, aux t)
    | AbsOnTy (x, k, t) -> AbsOnTy (x, k, aux t)
    | AppOnTy (f, x) -> AppOnTy (aux f, aux x)
  in
  aux

let rec kind_of_ty gamma = function
  | Ty name ->
      let x = List.find (fun x -> String.equal (fst x) name) gamma in
      snd (Option.default_delayed (fun () -> assert false) x)
  | Forall _
  | Fun _ -> Star
  | AbsOnTy (name, k, ret) -> KFun (k, kind_of_ty ((name, k) :: gamma) ret)
  | AppOnTy (f, x) ->
      let f = kind_of_ty gamma f in
      let x = kind_of_ty gamma x in
      match f with
      | KFun (p, r) when kind_equal p x -> r
      | KFun _
      | Star -> assert false

let equal gamma x y =
  let rec beta gamma = function
    | Ty name ->
        let x = List.find (fun x -> String.equal (fst x) name) gamma in
        fst (snd (Option.default_delayed (fun () -> assert false) x))
    | Fun (p, r) -> Fun (beta gamma p, beta gamma r)
    | Forall (name, k, t) -> Forall (name, k, beta ((name, (Ty name, k)) :: gamma) t)
    | AbsOnTy (name, k, t) -> AbsOnTy (name, k, beta ((name, (Ty name, k)) :: gamma) t)
    | AppOnTy (f, x) ->
        let f = beta gamma f in
        let x = beta gamma x in
        match f with
        | AbsOnTy (from, k, t) -> replace ~from ~ty:x t
        | Ty _
        | Fun _
        | Forall _
        | AppOnTy _ -> assert false
  in
  let rec aux = function
    | Fun (param, res), Fun (param', res') ->
        aux (param, param') && aux (res, res')
    | Ty x, Ty x' -> String.equal x x'
    | AbsOnTy (_, k1, t), AbsOnTy (_, k2, t')
    | Forall (_, k1, t), Forall (_, k2, t') when kind_equal k1 k2 -> aux (t, t')
    | AppOnTy _, _
    | _, AppOnTy _ -> assert false
    | AbsOnTy _, _
    | Forall _, _
    | Ty _, _
    | Fun _, _
    | Forall _, _ -> false
  in
  aux (beta gamma x, beta gamma y)

let rec size = function
  | Fun (_, t) -> succ (size t)
  | AbsOnTy _
  | AppOnTy _
  | Ty _ -> 0
  | Forall (_, _, t) -> size t
