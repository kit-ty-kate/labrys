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

open Containers
open Monomorphic.None

open DesugaredTree

let new_upper_name_to_variant ~current_module (loc, `NewUpperName name) =
  Ident.Variant.create ~loc current_module name
let new_upper_name_to_type ~current_module (loc, `NewUpperName name) =
  Ident.Type.create ~loc current_module name
let new_upper_name_to_exn ~current_module (loc, `NewUpperName name) =
  Ident.Exn.create ~loc current_module name
let new_upper_name_to_tyclass ~current_module (loc, `NewUpperName name) =
  Ident.TyClass.create ~loc current_module name

let create_name imports loc f local_f name =
  match Imports.Imports.find name imports with
  | modul ->
      let name = Utils.last name in
      f ~loc modul name
  | exception Not_found ->
      match name with
      | [name] -> local_f ~loc name
      | name -> Err.fail ~loc "Unbound identifier %s" (String.concat "." name)

let upper_name_to_variant imports (loc, `UpperName name) =
  create_name imports.Imports.variants loc Ident.Variant.create Ident.Variant.local_create name
let upper_name_to_type imports (loc, `UpperName name) =
  create_name imports.Imports.types loc Ident.Type.create Ident.Type.local_create name
let upper_name_to_exn imports (loc, `UpperName name) =
  create_name imports.Imports.exns loc Ident.Exn.create Ident.Exn.local_create name
let upper_name_to_tyclass imports (loc, `UpperName name) =
  create_name imports.Imports.tyclasses loc Ident.TyClass.create Ident.TyClass.local_create name

let lower_name_to_value imports (loc, `LowerName name) =
  create_name imports.Imports.values loc Ident.Name.create Ident.Name.local_create name
let lower_name_to_instance imports (loc, `LowerName name) =
  create_name imports.Imports.instances loc Ident.Instance.create Ident.Instance.local_create name

let new_lower_name_to_local_value ~allow_underscore = function
  | (loc, `NewLowerName name) ->
      Ident.Name.local_create ~loc name
  | (loc, `Underscore) when allow_underscore ->
      Builtins.underscore_loc loc
  | (loc, `Underscore) ->
      Err.fail ~loc "Wildcards are not allowed here"

let new_lower_name_to_local_instance ~allow_underscore = function
  | (loc, `NewLowerName name) ->
      Ident.Instance.local_create ~loc name
  | (loc, `Underscore) when allow_underscore ->
      Builtins.underscore_instance_loc loc
  | (loc, `Underscore) ->
      Err.fail ~loc "Wildcards are not allowed here"

let new_lower_name_to_value ~current_module ~allow_underscore = function
  | (loc, `NewLowerName name) ->
      Ident.Name.create ~loc current_module name
  | (loc, `Underscore) when allow_underscore ->
      Builtins.underscore_loc loc
  | (loc, `Underscore) ->
      Err.fail ~loc "Wildcards are not allowed here"

let new_lower_name_to_instance ~current_module ~allow_underscore = function
  | (loc, `NewLowerName name) ->
      Ident.Instance.create ~loc current_module name
  | (loc, `Underscore) when allow_underscore ->
      Builtins.underscore_instance_loc loc
  | (loc, `Underscore) ->
      Err.fail ~loc "Wildcards are not allowed here"

let new_lower_name_to_type_var = function
  | (loc, `NewLowerName name) ->
      Ident.TypeVar.local_create ~loc name
  | (loc, `Underscore) ->
      Builtins.underscore_type_var_loc loc

let unsugar_kind = Option.get Kinds.Star

let unsugar_eff imports (loc, l) =
  let aux = function
    | ParseTree.EffTy (name, args) ->
        let args = List.map (upper_name_to_exn imports) args in
        EffTy (upper_name_to_type imports name, args)
    | ParseTree.EffTyVar name ->
        EffTyVar (new_lower_name_to_type_var name)
  in
  (loc, List.map aux l)

let rec unsugar_tyclass imports (name, tyvars, args) =
  let name = upper_name_to_tyclass imports name in
  let tyvars =
    let aux (name, k) = (new_lower_name_to_type_var name, unsugar_kind k) in
    List.map aux tyvars
  in
  let args = List.map (unsugar_ty imports) args in
  (name, tyvars, args)

and unsugar_ty imports =
  let unsugar_forall ~loc ty args =
    let rec aux = function
      | (name, k) :: xs ->
          let name = new_lower_name_to_type_var name in
          (loc, Forall ((name, unsugar_kind k), aux xs))
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
          let name = new_lower_name_to_type_var name in
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
      let eff = Option.map (unsugar_eff imports) eff in
      (loc, Fun (unsugar_ty imports x, eff, unsugar_ty imports y))
  | (loc, ParseTree.Ty name) ->
      let name = upper_name_to_type imports name in
      (loc, Ty name)
  | (loc, ParseTree.TyVar name) ->
      let name = new_lower_name_to_type_var name in
      (loc, TyVar name)
  | (loc, ParseTree.Eff effects) ->
      let effects = unsugar_eff imports effects in
      (loc, Eff effects)
  | (loc, ParseTree.Forall (args, ty)) ->
      unsugar_forall ~loc ty args
  | (loc, ParseTree.TyClass (tyclass, eff, ty)) ->
      let tyclass = unsugar_tyclass imports tyclass in
      let eff = Option.map (unsugar_eff imports) eff in
      (loc, TyClass (tyclass, eff, unsugar_ty imports ty))
  | (loc, ParseTree.AbsOnTy (args, ty)) ->
      unsugar_absOnTy ~loc ty args
  | (loc, ParseTree.AppOnTy (x, y)) ->
      (loc, AppOnTy (unsugar_ty imports x, unsugar_ty imports y))

let unsugar_annot imports (annot, eff) =
  let eff = Option.map (unsugar_eff imports) eff in
  (unsugar_ty imports annot, eff)

let rec unsugar_pattern imports = function
  | ParseTree.TyConstr (loc, name, args) ->
      let name = upper_name_to_variant imports name in
      TyConstr (loc, name, List.map (unsugar_pattern imports) args)
  | ParseTree.Any name ->
      let name = new_lower_name_to_local_value ~allow_underscore:true name in
      Any name

let unsugar_instance imports (name, tys) =
  (upper_name_to_tyclass imports name, List.map (unsugar_ty imports) tys)

let unsugar_tyclass_app_arg imports = function
  | ParseTree.TyClassVariable name ->
      TyClassVariable (lower_name_to_instance imports name)
  | ParseTree.TyClassInstance instance ->
      TyClassInstance (unsugar_instance imports instance)

let unsugar_string ~loc s =
  let s = String.of_list s in
  let d = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec aux acc = match Uutf.decode d with
    | `Uchar c -> aux (c :: acc)
    | `End -> List.rev acc
    | `Malformed _ -> Err.fail ~loc "Malformed UTF-8 string"
    | `Await -> assert false
  in
  aux []

let unsugar_string ~loc s =
  let rec aux acc = function
    | [] -> List.rev acc
    | 0x5C::0x5C::xs -> aux (Uchar.of_char '\\' :: acc) xs
    | 0x5C::0x22::xs -> aux (Uchar.of_char '"' :: acc) xs
    | 0x5C::0x27::xs -> aux (Uchar.of_char '\'' :: acc) xs
    | 0x5C::0x6E::xs -> aux (Uchar.of_char '\n' :: acc) xs
    | 0x5C::0x72::xs -> aux (Uchar.of_char '\r' :: acc) xs
    | 0x5C::0x74::xs -> aux (Uchar.of_char '\t' :: acc) xs
    (*  | '\\' (num as n1) (num as n2) (num as n3)
        | "\\x" hexa hexa *) (* TODO *)
    | x::xs -> aux (Uchar.of_int x :: acc) xs
  in
  aux [] (List.map Uchar.to_int (unsugar_string ~loc s))

let unsugar_char ~loc s =
  match unsugar_string ~loc s with
  | [] -> Err.fail ~loc "A character cannot be empty"
  | [c] -> c
  | _ -> Err.fail ~loc "A character cannot contain several characters"

let rec unsugar_local_value imports options (name, is_rec, t) =
  let name = new_lower_name_to_local_value ~allow_underscore:true name in
  unsugar_let imports options name is_rec t

and unsugar_let imports options name is_rec (args, x) =
  let t = unsugar_args imports options args x in
  match is_rec with
  | ParseTree.NonRec -> (name, t)
  | ParseTree.Rec -> (name, (fst t, LetRec (name, t, (Builtins.unknown_loc, Val name))))

and unsugar_pat imports options (pattern, t) =
  (unsugar_pattern imports pattern, unsugar_t imports options t)

(* TODO: Allow full patterns but restrict here *)
and unsugar_try_pattern imports options (pattern, t) =
  let pattern =
    (upper_name_to_exn imports (fst pattern),
     List.map (new_lower_name_to_local_value ~allow_underscore:true) (snd pattern)
    )
  in
  (pattern, unsugar_t imports options t)

and unsugar_t imports options = function
  | (_, ParseTree.Abs (args, t)) ->
      if List.is_empty args then
        assert false;
      unsugar_args imports options args t
  | (loc, ParseTree.App (f, x)) ->
      (loc, App (unsugar_t imports options f, unsugar_t imports options x))
  | (loc, ParseTree.TApp (t, ty)) ->
      (loc, TApp (unsugar_t imports options t, unsugar_ty imports ty))
  | (loc, ParseTree.TyClassApp (t, x)) ->
      (loc, CApp (unsugar_t imports options t, unsugar_tyclass_app_arg imports x))
  | (loc, ParseTree.LowerVal name) ->
      let name = lower_name_to_value imports name in
      (loc, Val name)
  | (loc, ParseTree.UpperVal name) ->
      let name = upper_name_to_variant imports name in
      (loc, Var name)
  | (loc, ParseTree.PatternMatching (t, patterns)) ->
      (loc, PatternMatching (unsugar_t imports options t, List.map (unsugar_pat imports options) patterns))
  | (loc, ParseTree.Let (value, t)) ->
      let (name, x) = unsugar_local_value imports options value in
      (loc, Let (name, x, unsugar_t imports options t))
  | (loc, ParseTree.Fail (ty, (exn, args))) ->
      let exn = upper_name_to_exn imports exn in
      if Int.(List.length args > Config.max_fail_num_args) then
        Err.fail
          ~loc
          "Cannot handle more than %d parameters with fail"
          Config.max_fail_num_args;
      (loc, Fail (unsugar_ty imports ty, (exn, List.map (unsugar_t imports options) args)))
  | (loc, ParseTree.Try (t, patterns)) ->
      (loc, Try (unsugar_t imports options t, List.map (unsugar_try_pattern imports options) patterns))
  | (loc, ParseTree.Seq (x, y)) ->
      let name = Builtins.underscore in
      let ty = ((loc, Ty (Builtins.unit options)), None) in
      (loc, Let (name, (fst x, Annot (unsugar_t imports options x, ty)), unsugar_t imports options y))
  | (loc, ParseTree.Annot (t, ty)) ->
      (loc, Annot (unsugar_t imports options t, unsugar_annot imports ty))
  | (loc, ParseTree.Const (ParseTree.Int n)) ->
      (loc, Const (Int (int_of_string n)))
  | (loc, ParseTree.Const (ParseTree.Float n)) ->
      (loc, Const (Float (float_of_string n)))
  | (loc, ParseTree.Const (ParseTree.Char c)) ->
      let c = unsugar_char ~loc c in
      (loc, Const (Char c))
  | (loc, ParseTree.Const (ParseTree.String s)) ->
      let s = unsugar_string ~loc s in
      let buf = Buffer.create 64 in
      List.iter (Uutf.Buffer.add_utf_8 buf) s;
      (loc, Const (String (Buffer.contents buf)))

and unsugar_args imports options args (annot, t) =
  let rec aux = function
    | (loc, ParseTree.VArg (name, ty)) :: xs ->
        let name = new_lower_name_to_local_value ~allow_underscore:true name in
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
        let name = new_lower_name_to_type_var name in
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
    | (loc, ParseTree.Unit) :: xs ->
        let x =
          ParseTree.VArg
            ((loc, `Underscore), (loc, ParseTree.Ty (loc, Builtins.t_unit_name)))
        in
        aux ((loc, x) :: xs)
    | (loc, ParseTree.TyClassArg (name, tyclass)) :: xs ->
        let name = new_lower_name_to_local_instance ~allow_underscore:true name in
        let tyclass = unsugar_tyclass imports tyclass in
        let (ty_xs, xs) = aux xs in
        let ty_xs =
          let aux (ty_xs, eff) =
            let ty_xs = TyClass (tyclass, eff, ty_xs) in
            ((loc, ty_xs), None)
          in
          Option.map aux ty_xs
        in
        (ty_xs, (loc, CAbs ((name, tyclass), xs)))
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

let unsugar_variant ~current_module imports ~datatype ~args (ParseTree.Variant (name, tys)) =
  let name = new_upper_name_to_variant ~current_module name in
  let tys = List.map (unsugar_ty imports) tys in
  let uloc = Builtins.unknown_loc in
  let ty =
    let rec aux = function
      | [] -> List.fold_left (fun ty (x, _) -> AppOnTy ((uloc, ty), (uloc, TyVar x))) (Ty datatype) args
      | x::xs -> Fun (x, None, (uloc, aux xs))
    in
    List.fold_left (fun ty x -> Forall (x, (uloc, ty))) (aux tys) args
  in
  Variant (name, tys, (uloc, ty))

let unsugar_sig ~current_module imports (name, ty) =
  let name = new_lower_name_to_value ~current_module ~allow_underscore:false name in
  let ty = unsugar_ty imports ty in
  (name, ty)

let unsugar_variants ~current_module imports ~datatype ~args =
  List.map (unsugar_variant ~current_module imports ~datatype ~args)

let unsugar_variant_args args =
  let aux (x, k) = (new_lower_name_to_type_var x, unsugar_kind k) in
  List.map aux args

let unsugar_value ~current_module imports options (name, is_rec, t) =
  let name = new_lower_name_to_value ~current_module ~allow_underscore:true name in
  unsugar_let imports options name is_rec t

let unsugar_open ~current_module options = function
  | ParseTree.Source (loc, `UpperName modul) ->
      (loc, Module.create ~current_module modul, modul)
  | ParseTree.Library (loc, `UpperName modul) ->
      (loc, Module.library_create options modul, modul)

let create ~current_module options mimports =
  let rec aux imports = function
    | ParseTree.Value ((name, ParseTree.NonRec, _) as value) :: xs ->
        let value = unsugar_value ~current_module imports options value in
        let imports = Imports.add_value ~export:false name current_module imports in
        Value value :: aux imports xs
    | ParseTree.Value ((name, ParseTree.Rec, _) as value) :: xs ->
        let imports = Imports.add_value ~export:false name current_module imports in
        let value = unsugar_value ~current_module imports options value in
        Value value :: aux imports xs
    | ParseTree.Type (name, ty) :: xs ->
        let imports = Imports.add_type ~export:false name current_module imports in
        let name = new_upper_name_to_type ~current_module name in
        Type (name, unsugar_ty imports ty) :: aux imports xs
    | ParseTree.Foreign (cname, name, ty) :: xs ->
        let cname = unsugar_string ~loc:(fst name) cname in
        let cname =
          let buf = Buffer.create 32 in
          List.iter (Uutf.Buffer.add_utf_8 buf) cname;
          Buffer.contents buf
        in
        let imports' = Imports.add_value ~export:false name current_module imports in
        let name = new_lower_name_to_value ~current_module ~allow_underscore:false name in
        Foreign (cname, name, unsugar_ty imports ty) :: aux imports' xs
    | ParseTree.AbstractType (name, kind) :: xs ->
        let imports = Imports.add_type ~export:false name current_module imports in
        let name = new_upper_name_to_type ~current_module name in
        let kind = unsugar_kind kind in
        Datatype (name, kind, [], []) :: aux imports xs
    | ParseTree.Datatype (name, args, variants) :: xs ->
        let imports = Imports.add_type ~export:false name current_module imports in
        let kind = Kinds.from_list (List.map (fun (_, k) -> unsugar_kind k) args) in
        let name = new_upper_name_to_type ~current_module name in
        let args = unsugar_variant_args args in
        let imports =
          let aux imports (ParseTree.Variant (name, _)) =
            Imports.add_variant ~export:false name current_module imports
          in
          List.fold_left aux imports variants
        in
        let variants = unsugar_variants ~current_module imports ~datatype:name ~args variants in
        Datatype (name, kind, args, variants) :: aux imports xs
    | ParseTree.Exception (name, tys) :: xs ->
        let imports' = Imports.add_exn ~export:false name current_module imports in
        let name = new_upper_name_to_exn ~current_module name in
        Exception (name, List.map (unsugar_ty imports) tys) :: aux imports' xs
    | ParseTree.Open import :: xs ->
        let (loc, modul', modul) = unsugar_open ~current_module options import in
        if not (List.exists (Module.equal modul') mimports) then
          Err.fail ~loc "Module '%s' isn't imported" (Module.to_string modul');
        let imports = Imports.open_module modul imports in
        aux imports xs
    | ParseTree.Class (name, params, sigs) :: xs ->
        let imports' =
          let aux imports (name, _) =
            Imports.add_value ~export:false name current_module imports
          in
          let imports = Imports.add_tyclass ~export:false name current_module imports in
          List.fold_left aux imports sigs
        in
        let name = new_upper_name_to_tyclass ~current_module name in
        let params =
          let aux (name, k) =
            (new_lower_name_to_type_var name, unsugar_kind k)
          in
          List.map aux params
        in
        let sigs = List.map (unsugar_sig ~current_module imports) sigs in
        Class (name, params, sigs) :: aux imports' xs
    | ParseTree.Instance (tyclass, name, values) :: xs ->
        let tyclass = unsugar_instance imports tyclass in
        let (name, imports') = match name with
          | Some name ->
              let name' =
                new_lower_name_to_instance
                  ~current_module
                  ~allow_underscore:false
                  name
              in
              (Some name', Imports.add_instance ~export:false name current_module imports)
          | None ->
              (None, imports)
        in
        let values =
          List.map (unsugar_value ~current_module imports options) values
        in
        Instance (tyclass, name, values) :: aux imports' xs
    | [] ->
        []
  in
  aux

(* TODO: check "doublons" *)
let create_imports ~current_module options =
  let aux = function
    | ParseTree.Source (_, `UpperName name) ->
        Module.create ~current_module name
    | ParseTree.Library (_, `UpperName name) ->
        Module.library_create options name
  in
  List.map aux

let create_interface ~current_module options mimports imports interface =
  let rec aux imports local_imports acc = function
    | ParseTree.IVal ((name, _) as signature) :: xs ->
        let value = unsugar_sig ~current_module local_imports signature in
        let imports = Imports.add_value ~export:true name current_module imports in
        aux imports local_imports (InterfaceTree.Val value :: acc) xs
    | ParseTree.IAbstractType (name, k) :: xs ->
        let imports = Imports.add_type ~export:true name current_module imports in
        let local_imports = Imports.add_type ~export:false name current_module local_imports in
        let name = new_upper_name_to_type ~current_module name in
        aux imports local_imports (InterfaceTree.AbstractType (name, unsugar_kind k) :: acc) xs
    | ParseTree.IDatatype (name, args, variants) :: xs ->
        let imports = Imports.add_type ~export:true name current_module imports in
        let local_imports = Imports.add_type ~export:false name current_module local_imports in
        let kind = Kinds.from_list (List.map (fun (_, k) -> unsugar_kind k) args) in
        let name = new_upper_name_to_type ~current_module name in
        let args = unsugar_variant_args args in
        let imports =
          let aux imports (ParseTree.Variant (name, _)) =
            Imports.add_variant ~export:true name current_module imports
          in
          List.fold_left aux imports variants
        in
        let variants = unsugar_variants ~current_module local_imports ~datatype:name ~args variants in
        aux imports local_imports (InterfaceTree.Datatype (name, kind, args, variants) :: acc) xs
    | ParseTree.ITypeAlias (name, ty) :: xs ->
        let imports = Imports.add_type ~export:true name current_module imports in
        let local_imports' = Imports.add_type ~export:false name current_module local_imports in
        let name = new_upper_name_to_type ~current_module name in
        aux imports local_imports' (InterfaceTree.TypeAlias (name, unsugar_ty local_imports ty) :: acc) xs
    | ParseTree.IException (name, tys) :: xs ->
        let imports' = Imports.add_exn ~export:true name current_module imports in
        let name = new_upper_name_to_exn ~current_module name in
        aux imports' local_imports (InterfaceTree.Exception (name, List.map (unsugar_ty local_imports) tys) :: acc) xs
    | ParseTree.IOpen import :: xs ->
        let (loc, modul', modul) = unsugar_open ~current_module options import in
        if not (List.exists (Module.equal modul') mimports) then
          Err.fail ~loc "Module '%s' isn't imported" (Module.to_string modul');
        let local_imports = Imports.open_module modul local_imports in
        aux imports local_imports acc xs
    | ParseTree.IClass (name, params, sigs) :: xs ->
        let imports = Imports.add_tyclass ~export:true name current_module imports in
        let local_imports' = Imports.add_tyclass ~export:false name current_module local_imports in
        let name = new_upper_name_to_tyclass ~current_module name in
        let params =
          let aux (name, k) =
            (new_lower_name_to_type_var name, unsugar_kind k)
          in
          List.map aux params
        in
        let imports =
          let aux imports (name, _) =
            Imports.add_value ~export:true name current_module imports
          in
          List.fold_left aux imports sigs
        in
        let sigs = List.map (unsugar_sig ~current_module local_imports) sigs in
        aux imports local_imports' (InterfaceTree.Class (name, params, sigs) :: acc) xs
    | ParseTree.IInstance (tyclass, name) :: xs ->
        (* NOTE: No need to add the name in the interfaces *)
        let tyclass = unsugar_instance local_imports tyclass in
        let (name, imports, local_imports) = match name with
          | Some name ->
              let imports = Imports.add_instance ~export:true name current_module imports in
              let local_imports = Imports.add_instance ~export:false name current_module local_imports in
              let name = new_lower_name_to_instance ~current_module ~allow_underscore:false name in
              (Some name, imports, local_imports)
          | None ->
              (None, imports, local_imports)
        in
        aux imports local_imports (InterfaceTree.Instance (tyclass, name) :: acc) xs
    | [] ->
        (imports, acc)
  in
  let (imports, interface) = aux Imports.empty imports [] interface in
  (imports, List.rev interface)
