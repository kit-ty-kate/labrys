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

type name = Ident.Name.t

type var =
  | VLeaf
  | VNode of (int * var)

type mconstr =
  | MConstr of ((name * Ident.Type.t) * mconstr list)
  | MAny of (name * Ident.Type.t)

type 'a t = (mconstr * 'a) list

type code_index = int

type pattern =
  | Constr of (var * (name * Ident.Type.t) * pattern list)
  | Any of (var * (name * Ident.Type.t))

type matrix = (pattern list * code_index) list

let create =
  let rec aux gamma ty' = function
    | UnsugaredTree.Any name ->
        let gamma = Gamma.add_value name ty' gamma in
        (MAny (name, Types.head ty'), gamma)
    | UnsugaredTree.TyConstr (loc, name, args) ->
        let head_ty = Types.head ty' in
        let constructors =
          GammaMap.Constr.find head_ty gamma.Gamma.constructors
        in
        let constructors =
          Option.default_delayed (fun () -> assert false) constructors
        in
        begin match GammaMap.Index.find name constructors with
        | None ->
            Error.fail
              ~loc
              "Constructor '%s' not found in type '%s'"
              (Ident.Name.to_string name)
              (Ident.Type.to_string head_ty)
        | Some (ty, _) ->
            let aux (args, ty, gamma) = function
              | UnsugaredTree.PVal p ->
                  let (param_ty, effect, res_ty) = Types.apply ~loc ty in
                  if not (Effects.is_empty effect) then
                    assert false;
                  let (arg, gamma) = aux gamma param_ty p in
                  (arg :: args, res_ty, gamma)
              | UnsugaredTree.PTy pty ->
                  let (pty, kx) =
                    Types.of_parse_tree_kind
                      ~pure_arrow:`Allow
                      gamma.Gamma.types
                      gamma.Gamma.effects
                      pty
                  in
                  let (_, res) =
                    Types.apply_ty ~loc ~ty_x:pty ~kind_x:kx ty
                  in
                  (args, res, gamma)
            in
            let (args, ty, gamma) = List.fold_left aux ([], ty, gamma) args in
            let args = List.rev args in
            if not (Types.equal ty ty') then
              Error.fail
                ~loc
                "The type of the pattern is not equal to the type \
                 of the value matched";
            (MConstr ((name, Types.head ty), args), gamma)
        end
  in
  aux

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
