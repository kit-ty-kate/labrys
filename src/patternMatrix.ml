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

open Monomorphic_containers.Open

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

let var_compare x y =
  let rec aux = function
    | VLeaf, VLeaf -> 0
    | VNode (x, xs), VNode (y, ys) ->
        begin match Int.compare x y with
        | 0 -> aux (xs, ys)
        | c -> c
        end
    | VLeaf, VNode _ -> 1
    | VNode _, VLeaf -> -1
  in
  aux (x, y)

let create =
  let rec aux gamma ty' = function
    | UnsugaredTree.Any name ->
        let gamma = Gamma.add_value name ty' gamma in
        (MAny (name, fst (Types.head ty')), gamma)
    | UnsugaredTree.TyConstr (loc, name, args) ->
        let (head_ty, tail_ty) = Types.head ty' in
        let constructors = GammaMap.Constr.find head_ty gamma.Gamma.constructors in
        let (ty_args, constructors) = Option.get_lazy (fun () -> assert false) constructors in
        let (name, (tys, _)) = GammaMap.Index.find_binding ~head_ty name constructors in
        let aux (args, tys, gamma) p =
          match tys with
          | [] ->
              Err.fail ~loc "Too many parameters to this type constructor"
          | ty::xs ->
              let ty =
                let rec aux ty' = function
                  | from::xs, ty::ys ->
                      aux (Types.replace ~from ~ty ty') (xs, ys)
                  | [], [] ->
                      ty'
                  | _, [] | [], _ ->
                      assert false
                in
                aux ty (ty_args, tail_ty)
              in
              let (arg, gamma) = aux gamma ty p in
              (arg :: args, xs, gamma)
        in
        let (args, tys, gamma) = List.fold_left aux ([], tys, gamma) args in
        if not (List.is_empty tys) then
          Err.fail ~loc "Not enough parameters to this type constructor";
        let args = List.rev args in
        (MConstr ((name, head_ty), args), gamma)
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
