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

type name = Gamma.Name.t

type var =
  | VLeaf
  | VNode of (int * var)

type mconstr =
  | MConstr of ((name * Gamma.Type.t) * mconstr list)
  | MAny of (name * Gamma.Type.t)

type 'a t = (mconstr * 'a) list

type code_index = int

type pattern =
  | Constr of (var * (name * Gamma.Type.t) * pattern list)
  | Any of (var * (name * Gamma.Type.t))

type matrix = (pattern list * code_index) list

(* TODO: Remove those duplications *)
let type_error_aux ~loc =
  Error.fail
    ~loc
    "Error: This expression has type '%s' but an \
     expression was expected of type '%s'"

let type_error ~loc ~has ~expected =
  type_error_aux ~loc (TypesBeta.to_string has) (TypesBeta.to_string expected)

let function_type_error ~loc ~has ~expected =
  Error.fail
    ~loc
    "Error: Can't apply '%s' to a non-function type '%s'"
    (TypesBeta.to_string has)
    (TypesBeta.to_string expected)

let kind_missmatch ~loc ~has ~on =
  Error.fail
    ~loc
    "Cannot apply something with kind '%s' on '%s'"
    (Kinds.to_string has)
    (Kinds.to_string on)

let create ~loc gammaT gammaC =
  let rec aux gamma ty' = function
    | ParseTree.Any name ->
        let gamma = Gamma.Value.add name ty' gamma in
        (MAny (name, TypesBeta.head ty'), gamma)
    | ParseTree.TyConstr (name, args) ->
        let ty = Gamma.Index.find name gammaC in
        let (ty, _) = Option.default_delayed (fun () -> assert false) ty in
        let aux (args, ty, gamma) = function
          | ParseTree.PVal p ->
              let (param_ty, res_ty) = match ty with
                | TypesBeta.Fun (param, res) -> (param, res)
                | (TypesBeta.AppOnTy _ as ty)
                | (TypesBeta.Ty _ as ty) ->
                    function_type_error ~loc ~has:ty' ~expected:ty
                | TypesBeta.Forall (ty, _, _) ->
                    type_error_aux ~loc (TypesBeta.to_string ty') (Gamma.Type.to_string ty)
                | TypesBeta.AbsOnTy _ ->
                    assert false
              in
              let (arg, gamma) = aux gamma param_ty p in
              (arg :: args, res_ty, gamma)
          | ParseTree.PTy pty ->
              let (pty, kx) = TypesBeta.of_parse_tree_kind ~loc gammaT pty in
              begin match pty with
              | TypesBeta.Forall (from, k, ty) when Kinds.equal k kx ->
                  let ty = TypesBeta.replace ~from ~ty:pty ty in
                  (args, ty, gamma)
              | TypesBeta.Forall (_, k, _) -> kind_missmatch ~loc ~has:kx ~on:k
              | TypesBeta.Fun (ty, _) -> type_error ~loc ~has:pty ~expected:ty
              | (TypesBeta.AppOnTy _ as ty)
              | (TypesBeta.Ty _ as ty) -> function_type_error ~loc ~has:pty ~expected:ty
              | TypesBeta.AbsOnTy _ -> assert false
              end
        in
        let (args, ty, gamma) = List.fold_left aux ([], ty, gamma) args in
        if not (TypesBeta.equal ty ty') then
          Error.fail
            ~loc
            "The type of the pattern is not equal to the type of the value matched";
        (MConstr ((name, TypesBeta.head ty), List.rev args), gamma)
  in
  aux

let create ~loc gamma gammaT gammaC ty p = create ~loc gammaT gammaC gamma ty p

let succ_var = function
  | VLeaf -> VLeaf
  | VNode (i, var) -> VNode (succ i, var)

let split m =
  let rec change_row var = function
    | MConstr (name, args) :: xs ->
        let (args, names1) = change_row (VNode (0, var)) args in
        let (xs, names2) = change_row (succ_var var) xs in
        (Constr (var, name, args) :: xs, names1 @ names2)
    | MAny (name, ty) :: xs ->
        let (xs, names) = change_row (succ_var var) xs in
        (Any (var, (name, ty)) :: xs, (var, name) :: names)
    | [] ->
        ([], [])
  in
  let rec aux code_index = function
    | (row, branch) :: xs ->
        let (rows, branches) = aux (succ code_index) xs in
        let (row, names) = change_row VLeaf [row] in
        ((row, code_index) :: rows, (names, branch) :: branches)
    | [] ->
        ([], [])
  in
  aux 0 m
