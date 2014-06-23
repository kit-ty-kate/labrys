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

open TypedTree

type gamma =
  ( TypesBeta.t
  , [`Abstract of Kinds.t | `Alias of Types.t * Kinds.t]
  , (TypesBeta.t * int)
  , Gamma.Name.t list
  ) Gamma.t

let fail_rec_val ~loc =
  Error.fail ~loc "Recursive values are not allowed"

let rec well_formed_rec = function
  | ParseTree.Abs _ ->
      true
  | ParseTree.TAbs (_, _, t) ->
      well_formed_rec t
  | ParseTree.App _
  | ParseTree.TApp _
  | ParseTree.Val _
  | ParseTree.PatternMatching _
  | ParseTree.Let _
  | ParseTree.LetRec _ ->
      false

let rec aux gamma gammaT gammaC gammaD = function
  | ParseTree.Abs (loc, (name, ty), t) ->
      let ty = TypesBeta.of_parse_tree ~loc gammaT ty in
      let gamma = Gamma.Value.add name ty gamma in
      let (expr, ty_expr) = aux gamma gammaT gammaC gammaD t in
      let abs_ty = TypesBeta.func ~param:ty ~res:ty_expr in
      (Abs (name, expr), abs_ty)
  | ParseTree.TAbs (loc, (name, k), t) ->
      let gammaT = Gamma.Types.add ~loc name (`Abstract k) gammaT in
      let (expr, ty_expr) = aux gamma gammaT gammaC gammaD t in
      let abs_ty = TypesBeta.forall ~param:name ~kind:k ~res:ty_expr in
      (TAbs expr, abs_ty)
  | ParseTree.App (loc, f, x) ->
      let (f, ty_f) = aux gamma gammaT gammaC gammaD f in
      let (x, ty_x) = aux gamma gammaT gammaC gammaD x in
      let (param, res) = TypesBeta.apply ~loc ty_f in
      if TypesBeta.equal param ty_x then
        (App (f, x), res)
      else
        TypesBeta.Error.fail ~loc ~has:ty_x ~expected:param
  | ParseTree.TApp (loc, f, ty_x) ->
      let (f, ty_f) = aux gamma gammaT gammaC gammaD f in
      let (ty_x, kx) = TypesBeta.of_parse_tree_kind ~loc gammaT ty_x in
      let (param, res) = TypesBeta.apply_ty ~loc ~ty_x ~kind_x:kx ty_f in
      let res = TypesBeta.replace ~from:param ~ty:ty_x res in
      (TApp f, res)
  | ParseTree.Val (loc, name) ->
      begin match Gamma.Value.find name gamma with
      | None ->
          Error.fail
            ~loc
            "The value '%s' was not found in Γ"
            (Gamma.Name.to_string name)
      | Some ty ->
          (Val name, ty)
      end
  | ParseTree.PatternMatching (loc, t, patterns) ->
      let (t, ty) = aux gamma gammaT gammaC gammaD t in
      let (patterns, results, initial_ty) =
        let aux gamma = aux gamma gammaT gammaC gammaD in
        Pattern.create ~loc aux gamma gammaT gammaC gammaD ty patterns
      in
      (PatternMatching (t, results, patterns), initial_ty)
  | ParseTree.Let (name, t, xs) ->
      let (t, ty_t) = aux gamma gammaT gammaC gammaD t in
      let gamma = Gamma.Value.add name ty_t gamma in
      let (xs, ty_xs) = aux gamma gammaT gammaC gammaD xs in
      (Let (name, t, xs), ty_xs)
  | ParseTree.LetRec (loc, name, ty, t, xs) when well_formed_rec t ->
      let ty = TypesBeta.of_parse_tree ~loc gammaT ty in
      let gamma = Gamma.Value.add name ty gamma in
      let (t, ty_t) = aux gamma gammaT gammaC gammaD t in
      if not (TypesBeta.equal ty ty_t) then
        TypesBeta.Error.fail ~loc ~has:ty_t ~expected:ty;
      let (xs, ty_xs) = aux gamma gammaT gammaC gammaD xs in
      (LetRec (name, t, xs), ty_xs)
  | ParseTree.LetRec (loc, _, _, _, _) ->
      fail_rec_val ~loc

let transform_variants ~datatype gamma gammaT gammaC gammaD =
  let rec aux index = function
    | ParseTree.Variant (loc, name, ty) :: xs ->
        let ty = TypesBeta.of_parse_tree ~loc gammaT ty in
        if TypesBeta.check_if_returns_type ~datatype ty then
          let (xs, gamma, gammaC, gammaD) = aux (succ index) xs in
          let gamma = Gamma.Value.add name ty gamma in
          let gammaC = Gamma.Index.add name (ty, index) gammaC in
          let gammaD = Gamma.Constr.append datatype name gammaD in
          (Variant (name, TypesBeta.size ty) :: xs, gamma, gammaC, gammaD)
        else
          Error.fail
            ~loc
            "The variant '%s' doesn't return its type"
            (Gamma.Name.to_string name)
    | [] ->
        ([], gamma, gammaC, gammaD)
  in
  aux 0

let rec from_parse_tree gamma gammaT gammaC gammaD = function
  | ParseTree.Value (name, term) :: xs ->
      let (x, ty) = aux gamma gammaT gammaC gammaD term in
      let gamma = Gamma.Value.add name ty gamma in
      let (xs, gamma, gammaT, gammaC, gammaD) = from_parse_tree gamma gammaT gammaC gammaD xs in
      (Value (name, x) :: xs, gamma, gammaT, gammaC, gammaD)
  | ParseTree.RecValue (loc, name, ty, term) :: xs when well_formed_rec term ->
      let ty = TypesBeta.of_parse_tree ~loc gammaT ty in
      let gamma = Gamma.Value.add name ty gamma in
      let (x, ty_x) = aux gamma gammaT gammaC gammaD term in
      if not (TypesBeta.equal ty ty_x) then
        TypesBeta.Error.fail ~loc ~has:ty_x ~expected:ty;
      let (xs, gamma, gammaT, gammaC, gammaD) = from_parse_tree gamma gammaT gammaC gammaD xs in
      (RecValue (name, x) :: xs, gamma, gammaT, gammaC, gammaD)
  | ParseTree.RecValue (loc, _, _, _) :: _ ->
      fail_rec_val ~loc
  | ParseTree.Type (loc, name, ty) :: xs ->
      let ty = Types.from_parse_tree ~loc gammaT ty in
      let gammaT = Gamma.Types.add ~loc name (`Alias ty) gammaT in
      from_parse_tree gamma gammaT gammaC gammaD xs
  | ParseTree.Binding (loc, name, ty, binding) :: xs ->
      let ty = TypesBeta.of_parse_tree ~loc gammaT ty in
      let gamma = Gamma.Value.add name ty gamma in
      let (xs, gamma, gammaT, gammaC, gammaD) = from_parse_tree gamma gammaT gammaC gammaD xs in
      (Binding (name, binding) :: xs, gamma, gammaT, gammaC, gammaD)
  | ParseTree.Datatype (loc, name, kind, variants) :: xs ->
      let gammaT = Gamma.Types.add ~loc name (`Abstract kind) gammaT in
      let (variants, gamma, gammaC, gammaD) =
        transform_variants ~datatype:name gamma gammaT gammaC gammaD variants
      in
      let (xs, gamma, gammaT, gammaC, gammaD) = from_parse_tree gamma gammaT gammaC gammaD xs in
      (Datatype variants :: xs, gamma, gammaT, gammaC, gammaD)
  | [] ->
      ([], gamma, gammaT, gammaC, gammaD)

let from_parse_tree
      ~interface
      ({Gamma.values; types; indexes; constructors}, x) =
  let (res, gamma, gammaT, gammaC, gammaD) =
    from_parse_tree values types indexes constructors x
  in
  (* TODO: Improve *)
  begin match Gamma.subset interface (Gamma.of_gamma ~gamma ~gammaT ~gammaC ~gammaD) with
  | [] ->
      ()
  | diff ->
      (* TODO: Improve *)
      failwith
        (Printf.sprintf "Interface and implementation not compatible (%s)" (String.concat ", " (List.map Gamma.Name.to_string diff)))
  end;
  res
