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

open BatteriesExceptionless
open Monomorphic.None

open UnsugaredTree

let new_upper_name_to_value (loc, `NewUpperName name) =
  Ident.Name.create ~loc None name
let new_upper_name_to_type (loc, `NewUpperName name) =
  Ident.Type.create ~loc None name
let new_upper_name_to_exn (loc, `NewUpperName name) =
  Ident.Exn.create ~loc None name
let new_upper_name_to_eff (loc, `NewUpperName name) =
  Ident.Eff.create ~loc None name

let aux_to_value imports loc f = function
  | [] ->
      assert false
  | [name] ->
      f ~loc None name
  | name ->
      let (modul, name) = Utils.detach_last name in
      let aux (k, _) = List.eq String.equal k modul in
      begin match List.find aux imports with
      | None ->
          Error.fail ~loc "Unbound module %s" (String.concat "." modul)
      | Some (_, modul) ->
          f ~loc (Some modul) name
      end

let upper_name_to_value imports (loc, `UpperName name) =
  aux_to_value imports loc Ident.Name.create name

let upper_name_to_type imports (loc, `UpperName name) =
  aux_to_value imports loc Ident.Type.create name

let upper_name_to_exn imports (loc, `UpperName name) =
  aux_to_value imports loc Ident.Exn.create name

let new_lower_name_to_value ~allow_underscore = function
  | (loc, `NewLowerName name) -> Ident.Name.create ~loc None name
  | (loc, `Underscore) when allow_underscore -> Builtins.underscore_loc loc
  | (loc, `Underscore) ->
      Error.fail ~loc "Wildcards are not allowed here"

let lower_name_to_value imports (loc, `LowerName name) =
  aux_to_value imports loc Ident.Name.create name

let unsugar_kind = Option.default Kinds.Star

let unsugar_eff (loc, l) =
  let aux (name, args) =
    let name = new_upper_name_to_eff name in
    let args = List.map new_upper_name_to_exn args in
    (name, args)
  in
  (loc, List.map aux l)

let rec unsugar_ty imports =
  let unsugar_forall ~loc ty args =
    let rec aux = function
      | ParseTree.Eff name :: xs ->
          let name = new_upper_name_to_eff name in
          (loc, ForallEff (name, aux xs))
      | ParseTree.Typ (name, k) :: xs ->
          let name = new_upper_name_to_type name in
          (loc, Forall ((name, unsugar_kind k), aux xs))
      | ParseTree.TyClass (name, args) :: xs ->
          assert false
      | [] ->
          unsugar_ty imports ty
    in
    if List.is_empty args then
      assert false;
    aux args
  in
  let unsugar_absOnTy ~loc ty args =
    let rec aux = function
      | (name, k) :: xs ->
          let name = new_upper_name_to_type name in
          (loc, AbsOnTy ((name, unsugar_kind k), aux xs))
      | [] ->
          unsugar_ty imports ty
    in
    if List.is_empty args then
      assert false;
    aux args
  in
  function
  | (loc, ParseTree.Fun (x, eff, y)) ->
      let eff = Option.map unsugar_eff eff in
      (loc, Fun (unsugar_ty imports x, eff, unsugar_ty imports y))
  | (loc, ParseTree.Ty name) ->
      let name = upper_name_to_type imports name in
      (loc, Ty name)
  | (loc, ParseTree.Forall (args, ty)) ->
      unsugar_forall ~loc ty args
  | (loc, ParseTree.AbsOnTy (args, ty)) ->
      unsugar_absOnTy ~loc ty args
  | (loc, ParseTree.AppOnTy (x, y)) ->
      (loc, AppOnTy (unsugar_ty imports x, unsugar_ty imports y))

let unsugar_annot imports (annot, eff) =
  let eff = Option.map unsugar_eff eff in
  (unsugar_ty imports annot, eff)

let rec unsugar_pattern_arg imports = function
  | ParseTree.PVal pattern -> PVal (unsugar_pattern imports pattern)
  | ParseTree.PTy ty -> PTy (unsugar_ty imports ty)

and unsugar_pattern imports = function
  | ParseTree.TyConstr (loc, name, args) ->
      let name = upper_name_to_value imports name in
      TyConstr (loc, name, List.map (unsugar_pattern_arg imports) args)
  | ParseTree.Any name ->
      let name = new_lower_name_to_value ~allow_underscore:true name in
      Any name

let rec unsugar_pat imports options (pattern, t) = (unsugar_pattern imports pattern, unsugar_t imports options t)

(* TODO: Allow full patterns but restrict here *)
and unsugar_try_pattern imports options (pattern, t) =
  let pattern =
    (upper_name_to_exn imports (fst pattern),
     List.map (new_lower_name_to_value ~allow_underscore:true) (snd pattern)
    )
  in
  (pattern, unsugar_t imports options t)

and unsugar_t imports options = function
  | (_, ParseTree.Abs (args, t)) ->
      if List.is_empty args then
        assert false;
      unsugar_args imports options args None t
  | (loc, ParseTree.App (f, x)) ->
      (loc, App (unsugar_t imports options f, unsugar_t imports options x))
  | (loc, ParseTree.TApp (t, ty)) ->
      (loc, TApp (unsugar_t imports options t, unsugar_ty imports ty))
  | (loc, ParseTree.EApp (t, eff)) ->
      let eff = unsugar_eff eff in
      (loc, EApp (unsugar_t imports options t, eff))
  | (loc, ParseTree.LowerVal name) ->
      let name = lower_name_to_value imports name in
      (loc, Val name)
  | (loc, ParseTree.UpperVal name) ->
      let name = upper_name_to_value imports name in
      (loc, Val name)
  | (loc, ParseTree.PatternMatching (t, patterns)) ->
      (loc, PatternMatching (unsugar_t imports options t, List.map (unsugar_pat imports options) patterns))
  | (loc, ParseTree.Let ((name, is_rec, (args, (annot, x))), t)) ->
      let name = new_lower_name_to_value ~allow_underscore:true name in
      (loc, Let ((name, is_rec, unsugar_args imports options args annot x), unsugar_t imports options t))
  | (loc, ParseTree.Fail (ty, (exn, args))) ->
      let exn = upper_name_to_exn imports exn in
      (loc, Fail (unsugar_ty imports ty, (exn, List.map (unsugar_t imports options) args)))
  | (loc, ParseTree.Try (t, patterns)) ->
      (loc, Try (unsugar_t imports options t, List.map (unsugar_try_pattern imports options) patterns))
  | (loc, ParseTree.Seq (x, y)) ->
      let name = Builtins.underscore in
      let ty = ((loc, Ty (Builtins.t_unit options)), None) in
      (loc, Let ((name, NonRec, (fst x, Annot (unsugar_t imports options x, ty))), unsugar_t imports options y))
  | (loc, ParseTree.Annot (t, ty)) ->
      (loc, Annot (unsugar_t imports options t, unsugar_annot imports ty))

and unsugar_args imports options args annot t =
  let rec aux = function
    | (loc, ParseTree.VArg (name, ty)) :: xs ->
        let name = new_lower_name_to_value ~allow_underscore:true name in
        let ty = unsugar_ty imports ty in
        let (ty_xs, xs) = aux xs in
        let ty_xs =
          let aux (ty_xs, eff) =
            let ty_xs = Fun (ty, eff, ty_xs) in
            ((loc, ty_xs), None)
          in
          Option.map aux ty_xs
        in
        (ty_xs, (loc, Abs ((name, ty), xs)))
    | (loc, ParseTree.TArg (name, k)) :: xs ->
        let name = new_upper_name_to_type name in
        let ty = (name, unsugar_kind k) in
        let (ty_xs, xs) = aux xs in
        let ty_xs =
          let aux (ty_xs, eff) =
            let ty_xs = Forall (ty, ty_xs) in
            ((loc, ty_xs), eff)
          in
          Option.map aux ty_xs
        in
        (ty_xs, (loc, TAbs (ty, xs)))
    | (loc, ParseTree.EArg name) :: xs ->
        let name = new_upper_name_to_eff name in
        let (ty_xs, xs) = aux xs in
        let ty_xs =
          let aux (ty_xs, eff) =
            let ty_xs = ForallEff (name, ty_xs) in
            ((loc, ty_xs), eff)
          in
          Option.map aux ty_xs
        in
        (ty_xs, (loc, EAbs (name, xs)))
    | (loc, ParseTree.CArg (name, args)) :: xs ->
        assert false
    | (loc, ParseTree.Unit) :: xs ->
        let x =
          ParseTree.VArg
            ((loc, `Underscore), (loc, ParseTree.Ty (loc, Builtins.t_unit_name)))
        in
        aux ((loc, x) :: xs)
    | [] ->
        begin match annot with
        | Some annot ->
            let annot = unsugar_annot imports annot in
            (Some annot, (fst t, Annot (unsugar_t imports options t, annot)))
        | None ->
            (None, unsugar_t imports options t)
        end
  in
  match aux args with
  | (Some ty, t) -> (fst t, Annot (t, ty))
  | (None, t) -> t

let unsugar_variant imports (ParseTree.Variant (name, ty)) =
  let name = new_upper_name_to_value name in
  Variant (name, unsugar_ty imports ty)

let unsugar_variants imports = List.map (unsugar_variant imports)

let create imports options = function
  | ParseTree.Value (name, is_rec, (args, (ty, t))) ->
      let name = new_lower_name_to_value ~allow_underscore:false name in
      Value (name, is_rec, unsugar_args imports options args ty t)
  | ParseTree.Type (name, ty) ->
      let name = new_upper_name_to_type name in
      Type (name, unsugar_ty imports ty)
  | ParseTree.Binding (name, ty, content) ->
      let name = new_lower_name_to_value ~allow_underscore:false name in
      Binding (name, unsugar_ty imports ty, content)
  | ParseTree.Datatype (name, k, variants) ->
      let name = new_upper_name_to_type name in
      Datatype (name, unsugar_kind k, unsugar_variants imports variants)
  | ParseTree.Exception (name, tys) ->
      let name = new_upper_name_to_exn name in
      Exception (name, List.map (unsugar_ty imports) tys)

(* TODO: check "doublons" *)
let create_imports ~current_module options =
  let aux = function
    | ParseTree.Source (_, `UpperName name) ->
        (name, Module.create ~current_module name)
    | ParseTree.Library (_, `UpperName name) ->
        (name, Module.library_create options name)
  in
  List.map aux

let create ~current_module options imports tree =
  let imports = create_imports ~current_module options imports in
  let tree = List.map (create imports options) tree in
  (List.map snd imports, tree)

let create_interface imports = function
  | ParseTree.IVal (name, ty) ->
      let name = new_lower_name_to_value ~allow_underscore:false name in
      InterfaceTree.Val (name, unsugar_ty imports ty)
  | ParseTree.IAbstractType (name, k) ->
      let name = new_upper_name_to_type name in
      InterfaceTree.AbstractType (name, unsugar_kind k)
  | ParseTree.IDatatype (name, k, variants) ->
      let name = new_upper_name_to_type name in
      InterfaceTree.Datatype (name, unsugar_kind k, unsugar_variants imports variants)
  | ParseTree.ITypeAlias (name, ty) ->
      let name = new_upper_name_to_type name in
      InterfaceTree.TypeAlias (name, unsugar_ty imports ty)
  | ParseTree.IException (name, tys) ->
      let name = new_upper_name_to_exn name in
      InterfaceTree.Exception (name, List.map (unsugar_ty imports) tys)

let create_interface ~current_module options imports tree =
  let imports = create_imports ~current_module options imports in
  let tree = List.map (create_interface imports) tree in
  (List.map snd imports, tree)
