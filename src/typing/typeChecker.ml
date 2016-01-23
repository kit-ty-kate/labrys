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

open UntypedTree

let fail_rec_val ~loc_name =
  Err.fail ~loc:loc_name "Recursive values are not allowed"

let rec well_formed_rec = function
  | (_, UnsugaredTree.CAbs _)
  | (_, UnsugaredTree.Abs _) ->
      true
  | (_, UnsugaredTree.TAbs (_, t))
  | (_, UnsugaredTree.Annot (t, _))
  | (_, UnsugaredTree.Let (_, t)) ->
      well_formed_rec t
  | (_, UnsugaredTree.App _)
  | (_, UnsugaredTree.TApp _)
  | (_, UnsugaredTree.CApp _)
  | (_, UnsugaredTree.Val _)
  | (_, UnsugaredTree.Var _)
  | (_, UnsugaredTree.Const _)
  | (_, UnsugaredTree.PatternMatching _)
  | (_, UnsugaredTree.Fail _)
  | (_, UnsugaredTree.Try _) ->
      false

let rec get_ty_from_let = function
  | (_, UnsugaredTree.Annot (_, ty)) ->
      Some ty
  | (_, UnsugaredTree.Let (_, t)) ->
      get_ty_from_let t
  | (_, UnsugaredTree.CAbs _)
  | (_, UnsugaredTree.Abs _)
  | (_, UnsugaredTree.TAbs _)
  | (_, UnsugaredTree.App _)
  | (_, UnsugaredTree.TApp _)
  | (_, UnsugaredTree.CApp _)
  | (_, UnsugaredTree.Val _)
  | (_, UnsugaredTree.Var _)
  | (_, UnsugaredTree.Const _)
  | (_, UnsugaredTree.PatternMatching _)
  | (_, UnsugaredTree.Fail _)
  | (_, UnsugaredTree.Try _) ->
      None

let get_rec_ty ~loc_name = function
  | Some (ty, None) ->
      ty
  | Some (_, Some (loc, [])) ->
      Err.fail
        ~loc
        "This empty effect annotation is useless as functions cannot have \
         effects"
  | Some (_, Some (loc, _)) ->
      Err.fail ~loc "Functions doesn't have effects"
  | None ->
      Err.fail
        ~loc:loc_name
        "Recursive functions must have explicit return types"

let get_ty_from_let_rec ~loc_name ty =
  get_rec_ty ~loc_name (get_ty_from_let ty)

let check_type options ~loc_t ~ty:(ty, eff) ~ty_t ~effects gamma =
  let ty = Types.of_parse_tree ~pure_arrow:`Allow options gamma ty in
  if not (Types.equal ty ty_t) then
    Types.TyErr.fail ~loc_t ~has:ty_t ~expected:ty;
  begin match eff with
  | Some eff ->
      let eff = Effects.of_list options gamma eff in
      if not (Effects.equal [] eff effects) then
        Err.fail
          ~loc:loc_t
          "This expression has the effect %s but expected the effect %s"
          (Effects.to_string effects)
          (Effects.to_string eff);
  | None ->
      ()
  end

let check_type_opt options ~loc_t ~ty ~ty_t ~effects gamma =
  Option.iter (fun ty -> check_type options ~loc_t ~ty ~ty_t ~effects gamma) ty

let check_effects_forall ~loc_t options ~effect =
  (* TODO: Do I need only that to ensure type soundness with side effects ?
     Do I also need to check for exceptions ? *)
  if Effects.has_io options effect then
    Err.fail ~loc:loc_t "Cannot have IO effects under a forall"

let get_const options = function
  | UnsugaredTree.Int n -> (Int n, Builtins.int options)
  | UnsugaredTree.Float n -> (Float n, Builtins.float options)
  | UnsugaredTree.Char c -> (Char c, Builtins.char options)
  | UnsugaredTree.String s -> (String s, Builtins.string options)

let wrap_typeclass_apps ~loc gamma ~tyclasses ~f g =
  let rec aux f n = function
    | None::xs ->
        let name =
          Ident.Name.local_create ~loc:Builtins.unknown_loc "tyclass"
        in
        let name = Ident.Name.unique name n in
        Abs (name, aux (App (f, Val name)) (succ n) xs)
    | Some (tyclass, tys)::xs ->
        let tyclass' = GammaMap.TyClass.find tyclass gamma.Gamma.tyclasses in
        let name = Class.get_instance_name ~loc ~tyclass tys tyclass' in
        aux (App (f, Val name)) n xs
    | [] ->
        g f
  in
  aux f 1 tyclasses

let try_to_pattern l =
  let (branches, patterns) =
    let rec aux i = function
      | [] ->
          ([], [])
      | ((exn, args), t)::xs ->
          let used_vars =
            List.mapi (fun i x -> (PatternMatrix.exn_var i, x)) args
          in
          let (branches, patterns) = aux (succ i) xs in
          ((used_vars, t) :: branches, (exn, Pattern.Leaf i) :: patterns)
    in
    aux 0 l
  in
  let pattern =
    Pattern.Ptr (Pattern.Node (PatternMatrix.current_var, patterns))
  in
  let name = Ident.Name.local_create ~loc:Builtins.unknown_loc ".exn" in
  (name, PatternMatching (Val name, branches, Reraise name, pattern))

let rec aux options gamma = function
  | (_, UnsugaredTree.Abs ((name, ty), t)) ->
      let ty = Types.of_parse_tree ~pure_arrow:`Allow options gamma ty in
      let gamma = Gamma.add_value name ty gamma in
      let (expr, ty_expr, effect) = aux options gamma t in
      let abs_ty = PrivateTypes.Fun (ty, effect, ty_expr) in
      (Abs (name, expr), abs_ty, Effects.empty)
  | (_, UnsugaredTree.TAbs ((name, k), t)) ->
      let gamma = Gamma.add_type_var name k gamma in
      let (expr, ty_expr, effect) = aux options gamma t in
      check_effects_forall ~loc_t:(fst t) options ~effect;
      let abs_ty = Types.forall (name, k, ty_expr) in
      (expr, abs_ty, effect)
  | (_, UnsugaredTree.CAbs ((name, (tyclass, tyvars, args)), t)) ->
      let tyclass' = GammaMap.TyClass.find tyclass gamma.Gamma.tyclasses in
      let tyvars =
        let aux acc (name, k) = GammaMap.TypeVar.add name k acc in
        List.fold_left aux GammaMap.TypeVar.empty tyvars
      in
      let (gamma, args) =
        let loc = Ident.TyClass.loc tyclass in
        let f = Types.of_parse_tree_kind ~pure_arrow:`Forbid options in
        Class.get_params ~loc f gamma tyvars args tyclass'
      in
      let gamma = Gamma.add_named_instance name (tyclass, args) gamma in
      let (expr, ty_expr, effect) = aux options gamma t in
      let abs_ty = Types.tyclass ((tyclass, tyvars, args), effect, ty_expr) in
      (Abs (Ident.Instance.to_name name, expr), abs_ty, Effects.empty)
  | (loc, UnsugaredTree.App (f, x)) ->
      let loc_f = fst f in
      let loc_x = fst x in
      let (f, ty_f, effect1) = aux options gamma f in
      let (x, ty_x, effect2) = aux options gamma x in
      let (effect3, res, ty_x) = Types.apply ~loc_f ~loc_x ty_f ty_x in
      let (tyclasses, eff_f, res) = Types.extract_filled_tyclasses res in
      let (tyclasses_x, eff_x, _) = Types.extract_filled_tyclasses ty_x in
      let f =
        let aux f =
          let x =
            let aux x = function
              | None ->
                  assert false
              | Some (tyclass, tys) ->
                  let tyclass' = GammaMap.TyClass.find tyclass gamma.Gamma.tyclasses in
                  let name = Class.get_instance_name ~loc ~tyclass tys tyclass' in
                  App (x, Val name)
            in
            List.fold_left aux x tyclasses_x
          in
          App (f, x)
        in
        wrap_typeclass_apps ~loc gamma ~tyclasses ~f aux
      in
      (f, res, Effects.union5 effect1 effect2 effect3 eff_f eff_x)
  | (loc, UnsugaredTree.TApp (f, ty_x)) ->
      let loc_f = fst f in
      let loc_x = fst ty_x in
      let (f, ty_f, effect) = aux options gamma f in
      let (ty_x, kx) = Types.of_parse_tree_kind ~pure_arrow:`Allow options gamma ty_x in
      let res = Types.apply_ty ~loc_f ~loc_x ~ty_x ~kind_x:kx ty_f in
      let (tyclasses, eff_f, res) = Types.extract_filled_tyclasses res in
      let f = wrap_typeclass_apps ~loc gamma ~tyclasses ~f Fun.id in
      (f, res, Effects.union effect eff_f)
  | (loc, UnsugaredTree.CApp (f, x)) ->
      let (f, ty_f, effect1) = aux options gamma f in
      let (name, tyclass, args) = match x with
        | UnsugaredTree.TyClassVariable name ->
            let (tyclass, args) =
              GammaMap.Instance.find name gamma.Gamma.named_instances
            in
            (Ident.Instance.to_name name, tyclass, args)
        | UnsugaredTree.TyClassInstance (tyclass, tys) ->
            let aux x = fst (Types.of_parse_tree_kind ~pure_arrow:`Allow options gamma x) in
            let tys = List.map aux tys in
            let tyclass' = GammaMap.TyClass.find tyclass gamma.Gamma.tyclasses in
            let name =
              Class.get_instance_name ~loc:(Ident.TyClass.loc tyclass) ~tyclass tys tyclass'
            in
            (name, tyclass, tys)
      in
      (* TODO: Fix loc *)
      let (res, effect2) = Types.apply_tyclass ~loc_x:loc ty_f tyclass args in
      let (tyclasses, eff_f, res) = Types.extract_filled_tyclasses res in
      let f = App (f, Val name) in
      let f = wrap_typeclass_apps ~loc gamma ~tyclasses ~f Fun.id in
      (f, res, Effects.union3 effect1 effect2 eff_f)
  | (_, UnsugaredTree.Val name) ->
      let ty = GammaMap.Value.find name gamma.Gamma.values in
      (Val name, ty, Effects.empty)
  | (_, UnsugaredTree.Var name) ->
      let (idx, ty, len) =
        GammaMap.Variant.find name gamma.Gamma.variants
      in
      (Var (idx, len), ty, Effects.empty)
  | (loc, UnsugaredTree.PatternMatching (t, patterns)) ->
      let loc_t = fst t in
      let (t, ty, effect1) = aux options gamma t in
      if not (Types.is_value ty) then
        Err.fail ~loc:loc_t "This value cannot be matched";
      let (patterns, results, initial_ty, effect2) =
        Pattern.create ~loc (aux options) gamma ty patterns
      in
      let effect = Effects.union effect1 effect2 in
      (PatternMatching (t, results, Unreachable, patterns), initial_ty, effect)
  | (_, UnsugaredTree.Let ((name, UnsugaredTree.NonRec, t), xs)) ->
      let loc_t = fst t in
      let ty = get_ty_from_let t in
      let (t, ty_t, effect1) = aux options gamma t in
      check_type_opt options ~loc_t ~ty ~ty_t ~effects:effect1 gamma;
      let gamma = Gamma.add_value name ty_t gamma in
      let (xs, ty_xs, effect2) = aux options gamma xs in
      (Let ((name, NonRec, t), xs), ty_xs, Effects.union effect1 effect2)
  | (_, UnsugaredTree.Let ((name, UnsugaredTree.Rec, t), xs)) when well_formed_rec t ->
      let ty = get_ty_from_let_rec ~loc_name:(Ident.Name.loc name) t in
      let ty = Types.of_parse_tree ~pure_arrow:`Allow options gamma ty in
      let gamma = Gamma.add_value name ty gamma in
      let (t, _, effect1) = aux options gamma t in
      let (xs, ty_xs, effect2) = aux options gamma xs in
      (Let ((name, Rec, t), xs), ty_xs, Effects.union effect1 effect2)
  | (_, UnsugaredTree.Let ((name, UnsugaredTree.Rec, _), _)) ->
      fail_rec_val ~loc_name:(Ident.Name.loc name)
  | (loc, UnsugaredTree.Fail (ty, (exn, args))) ->
      let tys = GammaMap.Exn.find exn gamma.Gamma.exceptions in
      let ty = Types.of_parse_tree ~pure_arrow:`Allow options gamma ty in
      let (args, effects) =
        let aux (acc, effects) ty_exn arg =
          let loc_arg = fst arg in
          let (arg, ty_arg, eff) = aux options gamma arg in
          if not (Types.equal ty_arg ty_exn) then
            Types.TyErr.fail ~loc_t:loc_arg ~has:ty_arg ~expected:ty_exn;
          (arg :: acc, Effects.union eff effects)
        in
        try List.fold_left2 aux ([], Effects.empty) tys args with
        | Invalid_argument _ ->
            Err.fail
              ~loc
              "Cannot apply an exception partially"
      in
      let args = List.rev args in
      (Fail (exn, args), ty, Effects.add_exn exn effects)
  | (_, UnsugaredTree.Try (e, branches)) ->
      let (e, ty, effect) = aux options gamma e in
      let (branches, effect) =
        let aux (branches, effect) ((name, args), t) =
          let tys = GammaMap.Exn.find name gamma.Gamma.exceptions in
          if Int.Infix.(List.length args <> List.length tys) then
            Err.fail ~loc:(Ident.Exn.loc name) "Wrong number of argument";
          (branches @ [((name, args), t)], Effects.remove_exn name effect)
        in
        List.fold_left aux ([], effect) branches
      in
      let aux (acc, effect) ((name, args), t) =
        let loc_t = fst t in
        let (t, ty', eff) = aux options gamma t in
        (* TODO: Replace this pattern by Types.Check.equal *)
        if not (Types.equal ty ty') then
          Types.TyErr.fail ~loc_t ~has:ty' ~expected:ty;
        (((name, args), t) :: acc, Effects.union eff effect)
      in
      let (branches, effect) = List.fold_left aux ([], effect) branches in
      let branches = List.rev branches in
      (Try (e, try_to_pattern branches), ty, effect)
  | (_, UnsugaredTree.Annot (t, ty)) ->
      let loc_t = fst t in
      let (_, ty_t, effects) as res = aux options gamma t in
      check_type options ~loc_t ~ty ~ty_t ~effects gamma;
      res
  | (loc, UnsugaredTree.Const const) ->
      let (const, ty) = get_const options const in
      (Const const, Types.ty ~loc gamma ty, Effects.empty)

let transform_variants options ~datatype ~ty_args ~args gamma =
  let gamma' = List.fold_left (fun gamma (name, k) -> Gamma.add_type_var name k gamma) gamma args in
  let rec aux index = function
    | UnsugaredTree.Variant (name, tys, ty) :: xs ->
        let tys = List.map (Types.of_parse_tree ~pure_arrow:`Allow options gamma') tys in
        let ty = Types.of_parse_tree ~pure_arrow:`Allow options gamma ty in
        let gamma = aux (succ index) xs in
        let gamma = Gamma.add_variant name (index, ty, List.length tys) gamma in
        Gamma.add_constr datatype name ty_args (tys, index) gamma
    | [] ->
        gamma
  in
  aux 0

let is_main ~current_module ~has_main x =
  let b = Ident.Name.equal x (Builtins.main ~current_module) in
  if has_main && b then
    Err.fail ~loc:(Ident.Name.loc x) "There must be only one main";
  b

let check_effects ~current_module ~with_main ~has_main ~name options (t, ty, effects) =
  let is_main = with_main && is_main ~current_module ~has_main name in
  if not (is_main || Effects.is_empty effects) then
    Err.fail ~loc:(Ident.Name.loc name) "Effects are not allowed on toplevel";
  if is_main && not (Types.is_unit options ty) then
    Err.fail
      ~loc:(Ident.Name.loc name)
      "The main must have type '%s' but has type '%s'"
      (Ident.Type.to_string (Builtins.unit options))
      (Types.to_string ty);
  (is_main, t, ty)

let from_value ~current_module ~with_main ~has_main options gamma = function
  | (name, UnsugaredTree.NonRec, term) ->
      let loc_t = fst term in
      let ty = get_ty_from_let term in
      let (has_main, x, ty_t) = check_effects ~current_module ~with_main ~has_main ~name options (aux options gamma term) in
      check_type_opt options ~loc_t ~ty ~ty_t ~effects:Effects.empty gamma;
      let gamma = Gamma.add_value name ty_t gamma in
      ((name, NonRec, x), ty_t, has_main, gamma)
  | (name, UnsugaredTree.Rec, term) when well_formed_rec term ->
      let ty = get_ty_from_let_rec ~loc_name:(Ident.Name.loc name) term in
      let ty = Types.of_parse_tree ~pure_arrow:`Allow options gamma ty in
      let gamma = Gamma.add_value name ty gamma in
      let (has_main, x, _) = check_effects ~current_module ~with_main ~has_main ~name options (aux options gamma term) in
      ((name, Rec, x), ty, has_main, gamma)
  | (name, UnsugaredTree.Rec, _) ->
      fail_rec_val ~loc_name:(Ident.Name.loc name)

let get_foreign_type ~loc options =
  (* TODO: Unit -> X   -->   args = [] *)
  let fail loc =
    Err.fail ~loc "Such type is not handle in foreign declarations"
  in
  let fail_complex ty =
    Err.fail
      ~loc
      "Foreign declaration doesn't handle complexe types '%s'"
      (PrivateTypes.ty_to_string ty)
  in
  let arg_ty_map =
    [ (Builtins.int options, Int ())
    ; (Builtins.float options, Float ())
    ; (Builtins.char options, Char ())
    ; (Builtins.string options, String ())
    ]
  in
  let ret_ty_map =
    [ (Builtins.unit options, Void (Var (0, 0))) (* TODO: Clean the magic numbers *)
    ]
    @ List.map (fun (k, x) -> (k, Alloc x)) arg_ty_map
  in
  let rec aux acc = function
    | PrivateTypes.Fun (PrivateTypes.Ty name, eff, t) ->
        let x =
          match List.Assoc.get ~eq:Ident.Type.equal arg_ty_map name with
          | Some x -> x
          | None -> fail (Ident.Type.loc name)
        in
        begin match t with
        | PrivateTypes.Fun _ ->
            if not (Effects.is_empty eff) then
              Err.fail
                ~loc
                "Only empty effects are allowed on a non-final arrow";
            aux (x :: acc) t
        | PrivateTypes.Ty name ->
            if not (Effects.has_io options eff) then
              Err.fail
                ~loc
                "Bindings cannot be pure. \
                 All bindings have to use the IO effect on the final arrow";
            begin match List.Assoc.get ~eq:Ident.Type.equal ret_ty_map name with
            | Some y -> (y, List.rev (x :: acc))
            | None -> fail (Ident.Type.loc name)
            end
        | PrivateTypes.Eff _
        | PrivateTypes.Forall _
        | PrivateTypes.TyClass _
        | PrivateTypes.AbsOnTy _
        | PrivateTypes.AppOnTy _ ->
            fail_complex t
        | PrivateTypes.TyVar _ ->
            assert false
        end
    | PrivateTypes.Ty _ ->
        Err.fail ~loc "Cannot bind a global variable. Too unsafe."
    | PrivateTypes.Fun _
    | PrivateTypes.Eff _
    | PrivateTypes.Forall _
    | PrivateTypes.TyClass _
    | PrivateTypes.AbsOnTy _
    | PrivateTypes.AppOnTy _ as ty ->
        fail_complex ty
    | PrivateTypes.TyVar _ ->
        assert false
  in
  aux []

let rec from_parse_tree ~current_module ~with_main ~has_main options gamma = function
  | UnsugaredTree.Value value :: xs ->
      let (value, _, has_main, gamma) = from_value ~current_module ~with_main ~has_main options gamma value in
      let (xs, has_main, gamma) = from_parse_tree ~current_module ~with_main ~has_main options gamma xs in
      (Value value :: xs, has_main, gamma)
  | UnsugaredTree.Type (name, ty) :: xs ->
      let ty = Types.of_parse_tree_kind ~pure_arrow:`Forbid options gamma ty in
      let gamma = Gamma.add_type name (Types.Alias ty) gamma in
      from_parse_tree ~current_module ~with_main ~has_main options gamma xs
  | UnsugaredTree.Foreign (cname, name, ty) :: xs ->
      let ty = Types.of_parse_tree ~pure_arrow:`Allow options gamma ty in
      let gamma = Gamma.add_value name ty gamma in
      let ty = get_foreign_type ~loc:(Ident.Name.loc name) options ty in
      let (xs, has_main, gamma) = from_parse_tree ~current_module ~with_main ~has_main options gamma xs in
      (Foreign (cname, name, ty) :: xs, has_main, gamma)
  | UnsugaredTree.Datatype (name, kind, args, variants) :: xs ->
      let ty_args = List.map fst args in
      let gamma = Gamma.add_type name (Types.Abstract kind) gamma in
      let gamma = transform_variants options ~datatype:name ~ty_args ~args gamma variants in
      from_parse_tree ~current_module ~with_main ~has_main options gamma xs
  | UnsugaredTree.Exception (name, args) :: xs ->
      let args = List.map (Types.of_parse_tree ~pure_arrow:`Forbid options gamma) args in
      let gamma = Gamma.add_exception name args gamma in
      let (xs, has_main, gamma) = from_parse_tree ~current_module ~with_main ~has_main options gamma xs in
      (Exception name :: xs, has_main, gamma)
  | UnsugaredTree.Class (name, params, sigs) :: xs ->
      let sigs =
        let gamma =
          let aux gamma (name, k) = Gamma.add_type_var name k gamma in
          List.fold_left aux gamma params
        in
        let aux (name, ty) =
          (name, Types.of_parse_tree ~pure_arrow:`Forbid options gamma ty)
        in
        List.map aux sigs
      in
      let tyclass = Class.create params sigs in
      let gamma = Gamma.add_tyclass name tyclass gamma in
      let gamma =
        let aux gamma (name_sig, ty) =
          let ty = Types.tyclass_wrap name params ty in
          Gamma.add_value name_sig ty gamma
        in
        List.fold_left aux gamma sigs
      in
      let (xs, has_main, gamma) = from_parse_tree ~current_module ~with_main ~has_main options gamma xs in
      let (_, xs) =
        let aux (n, xs) (name, _) =
          let abs_name = Ident.Name.create ~loc:Builtins.unknown_loc current_module "0" in
          (succ n, Value (name, NonRec, Abs (abs_name, RecordGet (Val abs_name, n))) :: xs)
        in
        List.fold_left aux (0, xs) sigs
      in
      (xs, has_main, gamma)
  | UnsugaredTree.Instance ((tyclass, tys), name, values) :: xs ->
      let tyclass' = GammaMap.TyClass.find tyclass gamma.Gamma.tyclasses in
      let tys = List.map (Types.of_parse_tree_kind ~pure_arrow:`Forbid options gamma) tys in
      let values =
        let aux value =
          let (value, ty, _, _) = from_value ~current_module ~with_main ~has_main options gamma value in
          (value, ty)
        in
        List.map aux values
      in
      let (name', tys, tyclass') = Class.add_instance ~tyclass ~current_module tys tyclass' in
      let gamma = match name with
        | Some name -> Gamma.add_named_instance name (tyclass, tys) gamma
        | None -> gamma
      in
      let gamma = Gamma.replace_tyclass tyclass tyclass' gamma in
      let values = Class.get_values ~loc:(Ident.TyClass.loc tyclass) tys values tyclass' in
      let (xs, has_main, gamma) = from_parse_tree ~current_module ~with_main ~has_main options gamma xs in
      let values =
        let aux value t = Let (value, t) in
        let fields = List.map (fun (x, _, _) -> Val x) values in
        List.fold_right aux values (RecordCreate fields)
      in
      let xs = Option.maybe (fun name -> Value (Ident.Instance.to_name name, NonRec, Val name') :: xs) xs name in
      (Value (name', NonRec, values) :: xs, has_main, gamma)
  | [] ->
      ([], has_main, gamma)

let check ~modul ~interface ~with_main options gamma x =
  let (res, has_main, gamma) = from_parse_tree ~current_module:modul ~with_main ~has_main:false options gamma x in
  if with_main && not has_main then
    Err.fail_module "The 'main' hasn't been found in the main module";
  begin match Gamma.is_subset_of interface gamma with
  | [] ->
      ()
  | diff ->
      Err.fail_module
        "The implementation '%s' does not match the interface '%s'.\n\
         Differences: %s"
        (Module.impl modul)
        (Module.intf modul)
        (String.concat ", " diff);
  end;
  res
