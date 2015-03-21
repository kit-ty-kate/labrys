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
  | (_, UnsugaredTree.Abs _) ->
      true
  | (_, UnsugaredTree.TAbs (_, t))
  | (_, UnsugaredTree.Annot (t, _))
  | (_, UnsugaredTree.EAbs (_, t)) ->
      well_formed_rec t
  | (_, UnsugaredTree.App _)
  | (_, UnsugaredTree.TApp _)
  | (_, UnsugaredTree.EApp _)
  | (_, UnsugaredTree.Val _)
  | (_, UnsugaredTree.PatternMatching _)
  | (_, UnsugaredTree.Let _)
  | (_, UnsugaredTree.Fail _)
  | (_, UnsugaredTree.Try _) ->
      false

let get_ty_from_let = function
  | (_, UnsugaredTree.Annot (_, ty)) ->
      Some ty
  | (_, UnsugaredTree.Abs _)
  | (_, UnsugaredTree.TAbs _)
  | (_, UnsugaredTree.EAbs _)
  | (_, UnsugaredTree.App _)
  | (_, UnsugaredTree.TApp _)
  | (_, UnsugaredTree.EApp _)
  | (_, UnsugaredTree.Val _)
  | (_, UnsugaredTree.PatternMatching _)
  | (_, UnsugaredTree.Let _)
  | (_, UnsugaredTree.Fail _)
  | (_, UnsugaredTree.Try _) ->
      None

let get_rec_ty ~loc = function
  | Some (ty, None) ->
      ty
  | Some (_, Some []) ->
      Error.fail
        ~loc
        "This empty effect annotation is useless as functions cannot have \
         effects"
  | Some _ ->
      Error.fail ~loc "Functions doesn't have effects"
  | None ->
      Error.fail ~loc "Recursive values must have explicit return type"

let get_ty_from_let_rec ~loc ty =
  get_rec_ty ~loc (get_ty_from_let ty)

let check_type ~loc ~ty:(ty, eff) ~ty_t ~effects gamma =
  let ty =
    Types.of_parse_tree ~pure_arrow:`Allow gamma.Gamma.types gamma.Gamma.effects ty in
  if not (Types.equal ty ty_t) then
    Types.Error.fail ~loc ~has:ty_t ~expected:ty;
  begin match eff with
  | Some eff ->
      let eff = List.fold_right (Effects.add ~loc gamma.Gamma.effects) eff Effects.empty in
      if not (Effects.equal [] eff effects) then
        Error.fail
          ~loc
          "This expression has the effect %s but expected the effect %s"
          (Effects.to_string effects)
          (Effects.to_string eff);
  | None ->
      ()
  end

let check_type_opt ~loc ~ty ~ty_t ~effects gamma =
  Option.may (fun ty -> check_type ~loc ~ty ~ty_t ~effects gamma) ty

let rec aux gamma = function
  | (_loc, UnsugaredTree.Abs ((name, ty), t)) ->
      let ty = Types.of_parse_tree ~pure_arrow:`Allow gamma.Gamma.types gamma.Gamma.effects ty in
      let gamma = Gamma.add_value name ty gamma in
      let (expr, ty_expr, effect) = aux gamma t in
      let abs_ty = Types.func ~param:ty ~eff:effect ~res:ty_expr in
      (Abs (name, expr), abs_ty, Effects.empty)
  | (loc, UnsugaredTree.TAbs ((name, k), t)) ->
      let gamma = Gamma.add_type ~loc name (Types.Abstract k) gamma in
      let (expr, ty_expr, effect) = aux gamma t in
      (* TODO: Do I need only that to ensure type soundness with side effects ?
         Do I also need to check for exceptions ? *)
      if Effects.has_io effect then
        Error.fail ~loc "Cannot have IO effects under a forall";
      let abs_ty = Types.forall ~param:name ~kind:k ~res:ty_expr in
      (expr, abs_ty, effect)
  | (loc, UnsugaredTree.EAbs (name, t)) ->
      let gamma = Gamma.add_effect ~loc name gamma in
      let (expr, ty_expr, effect) = aux gamma t in
      (* TODO: Do I need only that to ensure type soundness with side effects ?
         Do I also need to check for exceptions ? *)
      if Effects.has_io effect then
        Error.fail ~loc "Cannot have IO effects under a forall";
      let abs_ty = Types.foralleff ~param:name ~res:ty_expr in
      (expr, abs_ty, effect)
  | (loc, UnsugaredTree.App (f, x)) ->
      let (f, ty_f, effect1) = aux gamma f in
      let (x, ty_x, effect2) = aux gamma x in
      let (param, effect3, res) = Types.apply ~loc ty_f in
      if Types.is_subset_of ty_x param then
        (App (f, x), res, Effects.union3 effect1 effect2 effect3)
      else
        Types.Error.fail ~loc ~has:ty_x ~expected:param
  | (loc, UnsugaredTree.TApp (f, ty_x)) ->
      let (f, ty_f, effect) = aux gamma f in
      let (ty_x, kx) = Types.of_parse_tree_kind ~pure_arrow:`Allow gamma.Gamma.types gamma.Gamma.effects ty_x in
      let (param, res) = Types.apply_ty ~loc ~ty_x ~kind_x:kx ty_f in
      let res = Types.replace ~from:param ~ty:ty_x res in
      (f, res, effect)
  | (loc, UnsugaredTree.EApp (f, eff)) ->
      let (f, ty_f, effect) = aux gamma f in
      let eff = Effects.of_list ~loc gamma.Gamma.effects eff in
      let (param, res) = Types.apply_eff ~loc ~eff ty_f in
      let res = Types.replace_eff ~from:param ~eff res in
      (f, res, effect)
  | (loc, UnsugaredTree.Val name) ->
      begin match GammaMap.Value.find name gamma.Gamma.values with
      | None ->
          Error.fail
            ~loc
            "The value '%s' was not found in Γ"
            (Ident.Name.to_string name)
      | Some ty ->
          (Val name, ty, Effects.empty)
      end
  | (loc, UnsugaredTree.PatternMatching (t, patterns)) ->
      let (t, ty, effect1) = aux gamma t in
      let (patterns, results, initial_ty, effect2) =
        Pattern.create ~loc aux gamma ty patterns
      in
      let effect = Effects.union effect1 effect2 in
      (PatternMatching (t, results, patterns), initial_ty, effect)
  | (loc, UnsugaredTree.Let ((name, UnsugaredTree.NonRec, t), xs)) ->
      let ty = get_ty_from_let t in
      let (t, ty_t, effect1) = aux gamma t in
      check_type_opt ~loc ~ty ~ty_t ~effects:effect1 gamma;
      let gamma = Gamma.add_value name ty_t gamma in
      let (xs, ty_xs, effect2) = aux gamma xs in
      (Let (name, t, xs), ty_xs, Effects.union effect1 effect2)
  | (loc, UnsugaredTree.Let ((name, UnsugaredTree.Rec, t), xs)) when well_formed_rec t ->
      let ty = get_ty_from_let_rec ~loc t in
      let ty = Types.of_parse_tree ~pure_arrow:`Allow gamma.Gamma.types gamma.Gamma.effects ty in
      let gamma = Gamma.add_value name ty gamma in
      let (t, ty_t, effect1) = aux gamma t in
      if not (Types.equal ty ty_t) then
        Types.Error.fail ~loc ~has:ty_t ~expected:ty;
      let (xs, ty_xs, effect2) = aux gamma xs in
      (LetRec (name, t, xs), ty_xs, Effects.union effect1 effect2)
  | (loc, UnsugaredTree.Let ((_, UnsugaredTree.Rec, _), _)) ->
      fail_rec_val ~loc
  | (loc, UnsugaredTree.Fail (ty, (exn, args))) ->
      let ty = Types.of_parse_tree ~pure_arrow:`Allow gamma.Gamma.types gamma.Gamma.effects ty in
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
          (Fail (exn, args), ty, Effects.add_exn exn effects)
      | None ->
          Error.fail
            ~loc
            "The exception '%s' is not defined in Γ"
            (Ident.Exn.to_string exn)
      end
  | (loc, UnsugaredTree.Try (e, branches)) ->
      let (e, ty, effect) = aux gamma e in
      let effect =
        List.fold_left
          (fun effect ((name, _), _) -> Effects.remove_exn ~loc name effect)
          effect
          branches
      in
      let aux (acc, effect) ((name, args), t) =
        let (t, ty', eff) = aux gamma t in
        if not (Types.equal ty ty') then
          Types.Error.fail ~loc ~has:ty' ~expected:ty;
        (((name, args), t) :: acc, Effects.union eff effect)
      in
      let (branches, effect) = List.fold_left aux ([], effect) branches in
      let branches = List.rev branches in
      (Try (e, branches), ty, effect)
  | (loc, UnsugaredTree.Annot (t, ty)) ->
      let (_, ty_t, effects) as res = aux gamma t in
      check_type ~loc ~ty ~ty_t ~effects gamma;
      res

let transform_variants ~datatype gamma =
  let rec aux index = function
    | UnsugaredTree.Variant (loc, name, ty) :: xs ->
        let ty = Types.of_parse_tree ~pure_arrow:`Allow gamma.Gamma.types gamma.Gamma.effects ty in
        if Types.check_if_returns_type ~datatype ty then
          let (xs, gamma) = aux (succ index) xs in
          let gamma = Gamma.add_value name ty gamma in
          let gamma = Gamma.add_constr datatype name (ty, index) gamma in
          (Variant (name, Types.size ty) :: xs, gamma)
        else
          Types.Error.fail_return_type ~loc name
    | [] ->
        ([], gamma)
  in
  aux 0

let is_main ~loc ~has_main x =
  let main = Ident.Name.of_list ["main"] in
  let b = Ident.Name.equal x main in
  if has_main && b then
    Error.fail ~loc "There must be only one main";
  b

let check_effects ~loc ~with_main ~has_main ~name (t, ty, effects) =
  let is_main = with_main && is_main ~loc ~has_main name in
  if not (is_main || Effects.is_empty effects) then
    Error.fail ~loc "Effects are not allowed on toplevel";
  if is_main && not (Types.is_unit ty) then
    Error.fail
      ~loc
      "The main must have type '%s' but has type '%s'"
      (Ident.Type.to_string Builtins.t_unit)
      (Types.to_string ty);
  (is_main, t, ty)

let rec from_parse_tree ~with_main ~has_main gamma = function
  | (loc, UnsugaredTree.Value (name, UnsugaredTree.NonRec, term)) :: xs ->
      let ty = get_ty_from_let term in
      let (has_main, x, ty_t) = check_effects ~loc ~with_main ~has_main ~name (aux gamma term) in
      check_type_opt ~loc ~ty ~ty_t ~effects:Effects.empty gamma;
      let gamma = Gamma.add_value name ty_t gamma in
      let (xs, has_main, gamma) = from_parse_tree ~with_main ~has_main gamma xs in
      (Value (name, x) :: xs, has_main, gamma)
  | (loc, UnsugaredTree.Value (name, UnsugaredTree.Rec, term)) :: xs when well_formed_rec term ->
      let ty = get_ty_from_let_rec ~loc term in
      let ty = Types.of_parse_tree ~pure_arrow:`Allow gamma.Gamma.types gamma.Gamma.effects ty in
      let gamma = Gamma.add_value name ty gamma in
      let (has_main, x, ty_x) = check_effects ~loc ~with_main ~has_main ~name (aux gamma term) in
      if not (Types.equal ty ty_x) then
        Types.Error.fail ~loc ~has:ty_x ~expected:ty;
      let (xs, has_main, gamma) = from_parse_tree ~with_main ~has_main gamma xs in
      (RecValue (name, x) :: xs, has_main, gamma)
  | (loc, UnsugaredTree.Value (_, UnsugaredTree.Rec, _)) :: _ ->
      fail_rec_val ~loc
  | (loc, UnsugaredTree.Type (name, ty)) :: xs ->
      let ty = Types.of_parse_tree_kind ~pure_arrow:`Forbid gamma.Gamma.types gamma.Gamma.effects ty in
      let gamma = Gamma.add_type ~loc name (Types.Alias ty) gamma in
      from_parse_tree ~with_main ~has_main gamma xs
  | (loc, UnsugaredTree.Binding (name, ty, binding)) :: xs ->
      let ty = Types.of_parse_tree ~pure_arrow:`Allow gamma.Gamma.types gamma.Gamma.effects ty in
      if not (Types.has_io ty) then
        Error.fail
          ~loc
          "The binding '%s' cannot be pure. \
           All bindings have to use the IO effect"
          (Ident.Name.to_string name);
      let gamma = Gamma.add_value name ty gamma in
      let (xs, has_main, gamma) = from_parse_tree ~with_main ~has_main gamma xs in
      (Binding (name, Types.size ty, binding) :: xs, has_main, gamma)
  | (loc, UnsugaredTree.Datatype (name, kind, variants)) :: xs ->
      let gamma = Gamma.add_type ~loc name (Types.Abstract kind) gamma in
      let (variants, gamma) = transform_variants ~datatype:name gamma variants in
      let (xs, has_main, gamma) = from_parse_tree ~with_main ~has_main gamma xs in
      (Datatype variants :: xs, has_main, gamma)
  | (loc, UnsugaredTree.Exception (name, args)) :: xs ->
      let args = List.map (Types.of_parse_tree ~pure_arrow:`Forbid gamma.Gamma.types gamma.Gamma.effects) args in
      let gamma = Gamma.add_exception ~loc name args gamma in
      let (xs, has_main, gamma) = from_parse_tree ~with_main ~has_main gamma xs in
      (Exception name :: xs, has_main, gamma)
  | [] ->
      ([], has_main, gamma)

let check ~interface ~with_main gamma x =
  let (res, has_main, gamma) = from_parse_tree ~with_main ~has_main:false gamma x in
  if with_main && not has_main then
    Error.fail_module "The 'main' hasn't been found in the main module";
  begin match Gamma.is_subset_of interface gamma with
  | [] ->
      ()
  | diff ->
      Error.fail_module
        "Interface and implementation not compatible (%s)"
        (String.concat ", " diff);
  end;
  res
