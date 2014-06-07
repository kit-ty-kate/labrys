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

module Matrix = struct
  type mconstr =
    | MConstr of ((name * Gamma.Type.t) * mconstr list)
    | MAny of name

  type 'a t = (mconstr * 'a) list

  type code_index = int

  type pattern =
    | Constr of ((name * Gamma.Type.t) * pattern list)
    | Any of name

  type matrix = (pattern list * code_index) list

  type ty =
    | AnyTy of name
    | SomeTy of TypesBeta.t

  let replace_ty ty = function
    | MConstr ((name, _), xs) -> MConstr ((name, TypesBeta.head ty), xs)
    | MAny _ as x -> x

  let create ~loc gammaT gammaC =
    let rec aux acc = function
      | ParseTree.Any name when List.is_empty acc ->
          (MAny name, AnyTy name)
      | ParseTree.Any name ->
          Error.fail ~loc "'%s' can't be applied to something" (Gamma.Name.to_string name)
      | ParseTree.TyConstr name ->
          let ty = Gamma.Index.find name gammaC in
          let ty = match ty with
            | Some x -> x
            | None ->
                Error.fail
                  ~loc
                  "The type constructor '%s' doesn't exists in Γ"
                  (Gamma.Name.to_string name)
          in
          (MConstr ((name, TypesBeta.head ty), acc), SomeTy ty)
      | ParseTree.PatternApp (f, x) ->
          let (x, ty_x) = aux [] x in
          let (f, ty_f) = aux (acc @ [x]) f in
          let ty_f = match ty_f with
            | SomeTy x -> x
            | AnyTy name ->
                Error.fail
                  ~loc
                  "Cannot apply something to the variable '%s'"
                  (Gamma.Name.to_string name)
          in
          let ty =
            match ty_x with
            | SomeTy ty_x ->
                begin match ty_f with
                | TypesBeta.Fun (ty, res) when TypesBeta.equal ty ty_x ->
                    res
                | TypesBeta.Fun (ty, _) ->
                    type_error ~loc ~has:ty_x ~expected:ty
                | (TypesBeta.AppOnTy _ as ty)
                | (TypesBeta.Ty _ as ty) ->
                    function_type_error ~loc ~has:ty_x ~expected:ty
                | TypesBeta.Forall (ty, _, _) ->
                    type_error_aux ~loc (TypesBeta.to_string ty_x) (Gamma.Type.to_string ty)
                | TypesBeta.AbsOnTy _ ->
                    assert false
                end
            | AnyTy name ->
                begin match ty_f with
                | TypesBeta.Fun (_, res) ->
                    res
                | (TypesBeta.AppOnTy _ as ty)
                | (TypesBeta.Forall _ as ty)
                | (TypesBeta.Ty _ as ty) ->
                    Error.fail
                      ~loc
                      "Cannot apply the variable '%s' to something with type '%s'"
                      (Gamma.Name.to_string name)
                      (TypesBeta.to_string ty)
                | TypesBeta.AbsOnTy _ ->
                    assert false
                end
          in
          (replace_ty ty f, SomeTy ty)
      | ParseTree.PatternTApp (x, param) ->
          let (x, ty) = aux acc x in
          begin match ty with
          | SomeTy ty ->
              let (param, kx) = Types.from_parse_tree ~loc gammaT param in
              let param = TypesBeta.of_ty param in
              begin match ty with
              | TypesBeta.Forall (from, k, ty) when Kinds.equal k kx ->
                  let ty = TypesBeta.replace ~from ~ty:param ty in
                  (replace_ty ty x, SomeTy ty)
              | TypesBeta.Forall (_, k, _) -> kind_missmatch ~loc ~has:kx ~on:k
              | TypesBeta.Fun (ty, _) -> type_error ~loc ~has:param ~expected:ty
              | (TypesBeta.AppOnTy _ as ty)
              | (TypesBeta.Ty _ as ty) -> function_type_error ~loc ~has:param ~expected:ty
              | TypesBeta.AbsOnTy _ -> assert false
              end
          | AnyTy name ->
              Error.fail
                ~loc
                "Cannot apply something to the variable '%s'"
                (Gamma.Name.to_string name)
          end
    in
    fun ty x ->
      let (res, ty') = aux [] x in
      begin match ty' with
      | SomeTy ty' ->
          if not (TypesBeta.equal ty ty') then
            Error.fail
              ~loc
              "The type of the pattern is not equal to the type of the value matched";
      | AnyTy _ ->
          ()
      end;
      res

  let create ~loc gammaT gammaC ty term p = [(create ~loc gammaT gammaC ty p, term)]

  let append ~loc gammaT gammaC ty term p patterns = patterns @ create ~loc gammaT gammaC ty term p

  let map f m = List.map (fun (constr, x) -> (constr, f x)) m

  let split m =
    let rec change_row = function
      | MConstr (name, args) :: xs ->
          let (args, names1) = change_row args in
          let (xs, names2) = change_row xs in
          (Constr (name, args) :: xs, names1 @ names2)
      | MAny name :: xs ->
          let (xs, names) = change_row xs in
          (Any name :: xs, name :: names)
      | [] ->
          ([], [])
    in
    let rec aux code_index = function
      | (row, branch) :: xs ->
          let (rows, branches) = aux (succ code_index) xs in
          let (row, names) = change_row [row] in
          ((row, code_index) :: rows, (names, branch) :: branches)
      | [] ->
          ([], [])
    in
    aux 0 m
end

type constr =
  | Constr of name
  | Any of name

type var =
  | VLeaf
  | VNode of (int * var)

type t =
  | Node of (var * (constr * t) * (constr * t) list)
  | Leaf of int

let specialize name m =
  let eq = Gamma.Name.equal in
  let size =
    let rec aux = function
      | (Matrix.Constr ((x, _), args) :: _, _) :: m when eq name x ->
          Int.max (List.length args) (aux m)
      | ([], _) :: m
      | (Matrix.Constr _ :: _, _) :: m
      | (Matrix.Any _ :: _, _) :: m ->
          aux m
      | [] ->
          0
    in
    aux m
  in
  let rec aux = function
    | (Matrix.Constr ((x, _), args) :: xs, code_index) :: m when eq name x ->
        (args @ xs, code_index) :: aux m
    | ([], _) :: m (* TODO: ??? *)
    | (Matrix.Constr _ :: _, _) :: m ->
        aux m
    | ((Matrix.Any _ as x) :: xs, code_index) :: m ->
        (List.make size x @ xs, code_index) :: aux m
    | [] ->
        []
  in
  aux m

let succ_var = function
  | VLeaf -> VLeaf
  | VNode (i, var) -> VNode (succ i, var)

let create gammaD m =
  let rec aux var m =
    match m with
    | (Matrix.Constr ((_, ty), _) :: _, code_index) :: _->
        let variants = Gamma.Constr.find ty gammaD in
        let variants =
          Option.default_delayed (fun () -> assert false) variants
        in
        let variants =
          let aux name =
            let xs =
              match specialize name m with
              | ([], code_index) :: _ -> Leaf code_index
              | [] -> Leaf code_index
              | m -> aux (VNode (0, var)) m
            in
            (Constr name, xs)
          in
          List.map aux variants
        in
        let (default, cases) =
          match variants with
          | [] -> assert false
          | x::xs -> (x, xs)
        in
        Node (var, default, cases)
    | (Matrix.Any name :: _, code_index) :: _ ->
        Leaf code_index (* TODO: Finish it *)
    | ([], _) :: _
    | [] ->
        assert false
  in
  aux VLeaf m
