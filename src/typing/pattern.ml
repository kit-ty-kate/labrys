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

module Matrix = PatternMatrix

type name = Ident.Name.t
type variant_name = Ident.Variant.t

type var = Matrix.var = private
  | VLeaf
  | VNode of (int * var)

type index = int

type constr = (variant_name * index)

type t =
  | Node of (Matrix.var * (constr * t) list)
  | Leaf of int

let are_any =
  let aux = function
    | Matrix.Any _ -> true
    | Matrix.Constr _ -> false
  in
  List.for_all aux

let specialize name m =
  let eq = Ident.Variant.equal in
  let size =
    let rec aux = function
      | (Matrix.Constr (_, (x, _), args) :: _, _) :: m when eq name x ->
          Int.Cmp.max (List.length args) (aux m)
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
    | (Matrix.Constr (_, (x, _), args) :: xs, code_index) :: m when eq name x ->
        (args @ xs, code_index) :: aux m
    | (Matrix.Constr _ :: _, _) :: m ->
        aux m
    | ((Matrix.Any _ as x) :: xs, code_index) :: m ->
        (List.replicate size x @ xs, code_index) :: aux m
    | ([], _) :: _ ->
        assert false
    | [] ->
        []
  in
  aux m

let create ~loc gammaD =
  let rec aux m = match m with
    | ((Matrix.Any _ :: _ as row), code_index) :: _ when are_any row ->
        Leaf code_index
    | (Matrix.Any (var, (_, ty)) :: _, _) :: _
    | (Matrix.Constr (var, (_, ty), _) :: _, _) :: _->
        let variants = GammaMap.Constr.find ty gammaD in
        let (_, variants) =
          Option.get_lazy (fun () -> assert false) variants
        in
        let variants =
          let aux name (_, index) acc =
            let xs =
              match specialize name m with
              | ([], code_index) :: _ -> Leaf code_index
              | [] ->
                  (* TODO: Be more precise *)
                  Err.fail
                    ~loc
                    "Pattern non-exostive on constructor '%s'"
                    (Ident.Variant.to_string name)
              | m -> aux m
            in
            ((name, index), xs) :: acc
          in
          GammaMap.Index.fold aux variants []
        in
        Node (var, variants)
    | ([], _) :: _
    | [] ->
        assert false
  in
  aux

let rec get_unused_cases results = function
  | Leaf i ->
      List.remove results ~x:i
  | Node (_, l) ->
      List.fold_left (fun r (_, p) -> get_unused_cases r p) results l

let create ~loc f gamma ty patterns =
  let (head, tail) = match patterns with
    | [] -> assert false
    | x::xs -> (x, xs)
  in
  let (initial_pattern, initial_ty, effect) =
    let (head_p, head_t) = head in
    let (pattern, gamma) = Matrix.create gamma ty head_p in
    let (term, ty_term, effect) = f gamma head_t in
    ([(pattern, term)], ty_term, effect)
  in
  let (patterns, effect) =
    let f (patterns, effects) (p, t) =
      let (pattern, gamma) = Matrix.create gamma ty p in
      let (loc_t, _) = t in
      let (t, has, effect) = f gamma t in
      if not (Types.equal has initial_ty) then
        Types.TyErr.fail ~loc_t ~has ~expected:initial_ty;
      ((pattern, t) :: patterns, Effects.union effect effects)
    in
    List.fold_left f (initial_pattern, effect) tail
  in
  let patterns = List.rev patterns in
  let (patterns, results) = Matrix.split patterns in
  let patterns = create ~loc gamma.Gamma.constructors patterns in
  let unused_cases =
    get_unused_cases (List.mapi (fun i _ -> i) results) patterns
  in
  if not (List.is_empty unused_cases) then
    (* TODO: Be more precise *)
    Err.fail
      ~loc
      "The pattern matching contains the following unused cases (%s)"
      (Utils.string_of_list (fun x -> string_of_int (succ x)) unused_cases);
  (patterns, results, initial_ty, effect)