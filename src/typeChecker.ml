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
  | ParseTree.LetRec _
  | ParseTree.Fail _
  | ParseTree.Try _ ->
      false

let rec aux gamma = function
  | ParseTree.Abs (loc, (name, ty), t) ->
      let ty = Types.of_parse_tree ~loc gamma.Gamma.types ty in
      let gamma = Gamma.add_value name ty gamma in
      let (expr, ty_expr, effect) = aux gamma t in
      let abs_ty = Types.func ~param:ty ~eff:effect ~res:ty_expr in
      (Abs (name, not (Effects.is_empty effect), expr), abs_ty, Effects.empty)
  | ParseTree.TAbs (loc, (name, k), t) ->
      let gamma = Gamma.add_type ~loc name (Types.Abstract k) gamma in
      let (expr, ty_expr, effect) = aux gamma t in
      let abs_ty = Types.forall ~param:name ~kind:k ~res:ty_expr in
      (TAbs expr, abs_ty, effect)
  | ParseTree.App (loc, f, x) ->
      let (f, ty_f, effect1) = aux gamma f in
      let (x, ty_x, effect2) = aux gamma x in
      let (param, effect3, res) = Types.apply ~loc ty_f in
      if Types.equal param ty_x then
        (App (f, not (Effects.is_empty effect3), x), res, Effects.union3 effect1 effect2 effect3)
      else
        Types.Error.fail ~loc ~has:ty_x ~expected:param
  | ParseTree.TApp (loc, f, ty_x) ->
      let (f, ty_f, effect) = aux gamma f in
      let (ty_x, kx) = Types.of_parse_tree_kind ~loc gamma.Gamma.types ty_x in
      let (param, res) = Types.apply_ty ~loc ~ty_x ~kind_x:kx ty_f in
      let res = Types.replace ~from:param ~ty:ty_x res in
      (TApp f, res, effect)
  | ParseTree.Val (loc, name) ->
      begin match GammaMap.Value.find name gamma.Gamma.values with
      | None ->
          Error.fail
            ~loc
            "The value '%s' was not found in Γ"
            (Ident.Name.to_string name)
      | Some ty ->
          (Val name, ty, Effects.empty)
      end
  | ParseTree.PatternMatching (loc, t, patterns) ->
      let (t, ty, effect1) = aux gamma t in
      let (patterns, results, initial_ty, effect2) =
        Pattern.create ~loc aux gamma ty patterns
      in
      let effect = Effects.union effect1 effect2 in
      (PatternMatching (t, results, patterns), initial_ty, effect)
  | ParseTree.Let (name, t, xs) ->
      let (t, ty_t, effect1) = aux gamma t in
      let gamma = Gamma.add_value name ty_t gamma in
      let (xs, ty_xs, effect2) = aux gamma xs in
      (Let (name, t, xs), ty_xs, Effects.union effect1 effect2)
  | ParseTree.LetRec (loc, name, ty, t, xs) when well_formed_rec t ->
      let ty = Types.of_parse_tree ~loc gamma.Gamma.types ty in
      let gamma = Gamma.add_value name ty gamma in
      let (t, ty_t, effect1) = aux gamma t in
      if not (Types.equal ty ty_t) then
        Types.Error.fail ~loc ~has:ty_t ~expected:ty;
      let (xs, ty_xs, effect2) = aux gamma xs in
      (LetRec (name, t, xs), ty_xs, Effects.union effect1 effect2)
  | ParseTree.LetRec (loc, _, _, _, _) ->
      fail_rec_val ~loc
  | ParseTree.Fail (loc, ty, (exn, args)) ->
      let ty = Types.of_parse_tree ~loc gamma.Gamma.types ty in
      begin match GammaMap.Exn.find exn gamma.Gamma.exceptions with
      | Some tys ->
          let (args, effects) =
            let aux (acc, effects) ty_exn arg =
              let (arg, ty_arg, eff) = aux gamma arg in
              if not (Types.equal ty_arg ty_exn) then
                Types.Error.fail ~loc ~has:ty_arg ~expected:ty_exn;
              (arg :: acc, Effects.union eff effects)
            in
            try List.fold_left2 aux ([], Effects.empty) tys args with
            | Invalid_argument _ ->
                Error.fail
                  ~loc
                  "Cannot fail with an exception applied partially"
          in
          let args = List.rev args in
          (Fail (exn, args), ty, Effects.add exn effects)
      | None ->
          Error.fail
            ~loc
            "The exception '%s' is not defined in Γ"
            (Ident.Name.to_string exn)
      end
  | ParseTree.Try (loc, e, branches) ->
      let (e, ty, effect) = aux gamma e in
      let effect =
        List.fold_left
          (fun effect (name, _) -> Effects.remove name effect)
          effect
          branches
      in
      let aux (acc, effect) (name, t) =
        let (t, ty', eff) = aux gamma t in
        if not (Types.equal ty ty') then
          Types.Error.fail ~loc ~has:ty' ~expected:ty;
        ((name, t) :: acc, Effects.union eff effect)
      in
      let (branches, effect) = List.fold_left aux ([], effect) branches in
      let branches = List.rev branches in
      (Try (e, not (Effects.is_empty effect), branches), ty, effect)

let transform_variants ~datatype gamma =
  let rec aux index = function
    | ParseTree.Variant (loc, name, ty) :: xs ->
        let ty = Types.of_parse_tree ~loc gamma.Gamma.types ty in
        if Types.check_if_returns_type ~datatype ty then
          let (xs, gamma) = aux (succ index) xs in
          let gamma = Gamma.add_value name ty gamma in
          let gamma = Gamma.add_constr datatype name (ty, index) gamma in
          (Variant (name, Types.size ty) :: xs, gamma)
        else
          Error.fail
            ~loc
            "The variant '%s' doesn't return its type"
            (Ident.Name.to_string name)
    | [] ->
        ([], gamma)
  in
  aux 0

let check_effects (t, ty, effects) =
  if not (Effects.is_empty effects) then
    (* TODO *)
    failwith "Effects are not allowed on toplevel";
  (t, ty)

let rec from_parse_tree gamma = function
  | ParseTree.Value (name, term) :: xs ->
      let (x, ty) = check_effects (aux gamma term) in
      let gamma = Gamma.add_value name ty gamma in
      let (xs, gamma) = from_parse_tree gamma xs in
      (Value (name, x) :: xs, gamma)
  | ParseTree.RecValue (loc, name, ty, term) :: xs when well_formed_rec term ->
      let ty = Types.of_parse_tree ~loc gamma.Gamma.types ty in
      let gamma = Gamma.add_value name ty gamma in
      let (x, ty_x) = check_effects (aux gamma term) in
      if not (Types.equal ty ty_x) then
        Types.Error.fail ~loc ~has:ty_x ~expected:ty;
      let (xs, gamma) = from_parse_tree gamma xs in
      (RecValue (name, x) :: xs, gamma)
  | ParseTree.RecValue (loc, _, _, _) :: _ ->
      fail_rec_val ~loc
  | ParseTree.Type (loc, name, ty) :: xs ->
      let ty = Types.of_parse_tree_kind ~loc gamma.Gamma.types ty in
      let gamma = Gamma.add_type ~loc name (Types.Alias ty) gamma in
      from_parse_tree gamma xs
  | ParseTree.Binding (loc, name, ty, binding) :: xs ->
      let ty = Types.of_parse_tree ~loc gamma.Gamma.types ty in
      let gamma = Gamma.add_value name ty gamma in
      let (xs, gamma) = from_parse_tree gamma xs in
      (Binding (name, binding) :: xs, gamma)
  | ParseTree.Datatype (loc, name, kind, variants) :: xs ->
      let gamma = Gamma.add_type ~loc name (Types.Abstract kind) gamma in
      let (variants, gamma) = transform_variants ~datatype:name gamma variants in
      let (xs, gamma) = from_parse_tree gamma xs in
      (Datatype variants :: xs, gamma)
  | ParseTree.Exception (loc, name, args) :: xs ->
      let args = List.map (Types.of_parse_tree ~loc gamma.Gamma.types) args in
      let gamma = Gamma.add_exception ~loc name args gamma in
      let (xs, gamma) = from_parse_tree gamma xs in
      (Exception name :: xs, gamma)
  | [] ->
      ([], gamma)

let from_parse_tree ~interface gamma x =
  let (res, gamma) = from_parse_tree gamma x in
  begin match Gamma.is_subset_of interface gamma with
  | [] ->
      ()
  | diff ->
      (* TODO: Improve *)
      failwith
        (Printf.sprintf "Interface and implementation not compatible (%s)" (String.concat ", " diff))
  end;
  res
