(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open DesugaredTree

let foldl = List.fold_left
let foldr = List.fold_right

let new_upper_name_to_variant ~current_module (loc, `NewUpperName name) =
  Ident.Variant.create ~loc current_module name
let new_upper_name_to_type ~current_module (loc, `NewUpperName name) =
  Ident.Type.create ~loc current_module name
let new_upper_name_to_exn ~current_module (loc, `NewUpperName name) =
  Ident.Exn.create ~loc current_module name
let new_upper_name_to_tyclass ~current_module (loc, `NewUpperName name) =
  Ident.TyClass.create ~loc current_module name

module type ID = module type of Ident.Name

let create_name (type a) imports loc (module Id : ID with type t = a) name =
  match Imports.Imports.find name imports with
  | modul ->
      let name = Utils.last name in
      Id.create ~loc modul name
  | exception Not_found ->
      match name with
      | [name] -> Id.local_create ~loc name
      | name -> Err.fail ~loc "Unbound identifier %s" (String.concat "." name)

let upper_name_to_variant imports (loc, `UpperName name) =
  create_name imports.Imports.variants loc (module Ident.Variant) name
let upper_name_to_type imports (loc, `UpperName name) =
  create_name imports.Imports.types loc (module Ident.Type) name
let upper_name_to_exn imports (loc, `UpperName name) =
  create_name imports.Imports.exns loc (module Ident.Exn) name
let upper_name_to_tyclass imports (loc, `UpperName name) =
  create_name imports.Imports.tyclasses loc (module Ident.TyClass) name

let lower_name_to_value imports (loc, `LowerName name) =
  create_name imports.Imports.values loc (module Ident.Name) name
let lower_name_to_instance imports (loc, `LowerName name) =
  create_name imports.Imports.instances loc (module Ident.Instance) name

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

let desugar_kind = Option.get_or ~default:KStar

let desugar_t_value (name, k) =
  let name = new_lower_name_to_type_var name in
  let k = desugar_kind k in
  (name, k)

let rec desugar_tyclass imports (name, tyvars, args) =
  let name = upper_name_to_tyclass imports name in
  let tyvars = List.map desugar_t_value tyvars in
  let args = List.map (desugar_ty imports) args in
  (name, tyvars, args)

and desugar_eff imports (loc, l) =
  (loc, List.map (desugar_ty imports) l)

and desugar_ty imports = function
  | (loc, ParseTree.Fun (x, eff, y)) ->
      let eff = Option.map (desugar_eff imports) eff in
      (loc, Fun (desugar_ty imports x, eff, desugar_ty imports y))
  | (loc, ParseTree.Ty name) ->
      let name = upper_name_to_type imports name in
      (loc, Ty name)
  | (loc, ParseTree.TyVar name) ->
      let name = new_lower_name_to_type_var name in
      (loc, TyVar name)
  | (loc, ParseTree.Eff effects) ->
      let effects = desugar_eff imports effects in
      (loc, Eff effects)
  | (loc, ParseTree.Forall (args, ty)) ->
      assert (not (List.is_empty args));
      desugar_tys ~loc ~ty imports (fun x -> Forall x) args
  | (loc, ParseTree.TyClass (tyclass, eff, ty)) ->
      let tyclass = desugar_tyclass imports tyclass in
      let eff = Option.map (desugar_eff imports) eff in
      (loc, TyClass (tyclass, eff, desugar_ty imports ty))
  | (loc, ParseTree.AbsOnTy (args, ty)) ->
      assert (not (List.is_empty args));
      desugar_tys ~loc ~ty imports (fun x -> AbsOnTy x) args
  | (loc, ParseTree.AppOnTy (x, y)) ->
      (loc, AppOnTy (desugar_ty imports x, desugar_ty imports y))

and desugar_tys ~loc ~ty imports f = function
  | t_value :: xs ->
      let t_value = desugar_t_value t_value in
      let ty = desugar_tys ~loc ~ty imports f xs in
      (loc, f (t_value, ty))
  | [] ->
      desugar_ty imports ty

let desugar_annot imports (annot, eff) =
  let annot = desugar_ty imports annot in
  let eff = Option.map (desugar_ty imports) eff in
  (annot, eff)

let rec desugar_pattern imports = function
  | ParseTree.TyConstr (loc, name, args) ->
      let name = upper_name_to_variant imports name in
      let args = List.map (desugar_pattern imports) args in
      TyConstr (loc, name, args)
  | ParseTree.Any name ->
      let name = new_lower_name_to_local_value ~allow_underscore:true name in
      Any name

let desugar_instance imports (name, tys) =
  let name = upper_name_to_tyclass imports name in
  let tys = List.map (desugar_ty imports) tys in
  (name, tys)

let desugar_tyclass_app_arg imports = function
  | ParseTree.TyClassVariable name ->
      let name = lower_name_to_instance imports name in
      TyClassVariable name
  | ParseTree.TyClassInstance instance ->
      let instance = desugar_instance imports instance in
      TyClassInstance instance

let desugar_uchar_list ~loc s =
  let s = String.of_list s in
  let d = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec aux acc = match Uutf.decode d with
    | `Uchar c -> aux (c :: acc)
    | `End -> List.rev acc
    | `Malformed _ -> Err.fail ~loc "Malformed UTF-8 string"
    | `Await -> assert false
  in
  aux []

let desugar_uchar_list ~loc s =
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
  aux [] (List.map Uchar.to_int (desugar_uchar_list ~loc s))

let desugar_string ~loc s =
  let s = desugar_uchar_list ~loc s in
  let buf = Buffer.create 64 in
  List.iter (Uutf.Buffer.add_utf_8 buf) s;
  Buffer.contents buf

let desugar_char ~loc s =
  match desugar_uchar_list ~loc s with
  | [] -> Err.fail ~loc "A character cannot be empty"
  | [c] -> c
  | _ -> Err.fail ~loc "A character cannot contain several characters"

let desugar_const ~loc = function
  | ParseTree.Int n ->
      let n = int_of_string n in
      Int n
  | ParseTree.Float n ->
      let n = float_of_string n in
      Float n
  | ParseTree.Char c ->
      let c = desugar_char ~loc c in
      Char c
  | ParseTree.String s ->
      let s = desugar_string ~loc s in
      String s

let desugar_try_arg_to_name imports = function
  | ParseTree.TyConstr (loc, _, _) ->
      Err.fail ~loc "Full pattern-matching is not supported"
  | ParseTree.Any name ->
      new_lower_name_to_local_value ~allow_underscore:true name

let rec desugar_local_value imports options (name, is_rec, t) =
  let name = new_lower_name_to_local_value ~allow_underscore:true name in
  desugar_let imports options name is_rec t

and desugar_let imports options name is_rec (args, x) =
  let t = desugar_args imports options args x in
  let unknown_loc = Builtins.unknown_loc in
  match is_rec with
  | ParseTree.NonRec -> (name, t)
  | ParseTree.Rec -> (name, (fst t, LetRec (name, t, (unknown_loc, Val name))))

and desugar_pat imports options (pattern, t) =
  let pattern = desugar_pattern imports pattern in
  let t = desugar_t imports options t in
  (pattern, t)

and desugar_try_pattern imports options = function
  | (ParseTree.TyConstr (loc, exn, args), t) ->
      let exn = upper_name_to_exn imports exn in
      let args = List.map (desugar_try_arg_to_name imports) args in
      let t = desugar_t imports options t in
      ((exn, args), t)
  | (ParseTree.Any (loc, _), _) ->
      Err.fail ~loc "Wildcard patterns are not supported here"

and desugar_t imports options = function
  | (_, ParseTree.Abs (args, t)) ->
      assert (not (List.is_empty args));
      desugar_args imports options args t
  | (loc, ParseTree.App (f, x)) ->
      let f = desugar_t imports options f in
      let x = desugar_t imports options x in
      (loc, App (f, x))
  | (loc, ParseTree.TApp (t, ty)) ->
      let t = desugar_t imports options t in
      let ty = desugar_ty imports ty in
      (loc, TApp (t, ty))
  | (loc, ParseTree.TyClassApp (t, x)) ->
      let t = desugar_t imports options t in
      let x = desugar_tyclass_app_arg imports x in
      (loc, CApp (t, x))
  | (loc, ParseTree.LowerVal name) ->
      let name = lower_name_to_value imports name in
      (loc, Val name)
  | (loc, ParseTree.UpperVal name) ->
      let name = upper_name_to_variant imports name in
      (loc, Var name)
  | (loc, ParseTree.PatternMatching (t, patterns)) ->
      assert (not (List.is_empty patterns));
      let t = desugar_t imports options t in
      let patterns = List.map (desugar_pat imports options) patterns in
      (loc, PatternMatching (t, patterns))
  | (loc, ParseTree.Let (value, t)) ->
      let (name, x) = desugar_local_value imports options value in
      let t = desugar_t imports options t in
      (loc, Let (name, x, t))
  | (loc, ParseTree.Fail (ty, (exn, args))) ->
      let ty = desugar_ty imports ty in
      let exn = upper_name_to_exn imports exn in
      let args = List.map (desugar_t imports options) args in
      (loc, Fail (ty, (exn, args)))
  | (loc, ParseTree.Try (t, patterns)) ->
      assert (not (List.is_empty patterns));
      let t = desugar_t imports options t in
      let patterns = List.map (desugar_try_pattern imports options) patterns in
      (loc, Try (t, patterns))
  | (loc, ParseTree.Seq (x, y)) ->
      let name = Builtins.underscore in
      let x =
        let ty = ((loc, Ty (Builtins.unit options)), None) in
        (fst x, Annot (desugar_t imports options x, ty))
      in
      let y = desugar_t imports options y in
      (loc, Let (name, x, y))
  | (loc, ParseTree.Annot (t, ty)) ->
      let t = desugar_t imports options t in
      let ty = desugar_annot imports ty in
      (loc, Annot (t, ty))
  | (loc, ParseTree.Const c) ->
      let c = desugar_const ~loc c in
      (loc, Const c)

(* TODO: Clean *)
and desugar_args imports options args (annot, t) =
  let rec aux = function
    | (loc, ParseTree.VArg (name, ty)) :: xs ->
        let name = new_lower_name_to_local_value ~allow_underscore:true name in
        let ty = desugar_ty imports ty in
        (loc, Abs ((name, ty), aux xs))
    | (loc, ParseTree.TArg t_value) :: xs ->
        let t_value = desugar_t_value t_value in
        (loc, TAbs (t_value, aux xs))
    | (loc, ParseTree.Unit) :: xs ->
        let x =
          ParseTree.VArg
            ((loc, `Underscore), (loc, ParseTree.Ty (loc, Builtins.t_unit_name)))
        in
        aux ((loc, x) :: xs)
    | (loc, ParseTree.TyClassArg (name, tyclass)) :: xs ->
        let name = new_lower_name_to_local_instance ~allow_underscore:true name in
        let tyclass = desugar_tyclass imports tyclass in
        (loc, CAbs ((name, tyclass), aux xs))
    | [] ->
        begin match annot with
        | Some annot ->
            let annot = desugar_annot imports annot in
            (fst t, Annot (desugar_t imports options t, annot))
        | None ->
            desugar_t imports options t
        end
  in
  aux args

let desugar_variant ~current_module imports ~datatype ~args (name, tys) =
  let name = new_upper_name_to_variant ~current_module name in
  let tys = List.map (desugar_ty imports) tys in
  let ty =
    let uloc = Builtins.unknown_loc in
    let ty = (uloc, Ty datatype) in
    foldl (fun ty (arg, _) -> (uloc, AppOnTy (ty, (uloc, TyVar arg)))) ty args
    |> foldr (fun x ty -> (uloc, Fun (x, None, ty))) tys
    |> foldr (fun arg ty -> (uloc, Forall (arg, ty))) args
  in
  (name, tys, ty)

let desugar_variants ~current_module imports ~datatype ~args =
  List.map (desugar_variant ~current_module imports ~datatype ~args)

let desugar_sig ~current_module imports (name, ty) =
  let name = new_lower_name_to_value ~current_module ~allow_underscore:false name in
  let ty = desugar_ty imports ty in
  (name, ty)

let desugar_variant_args args =
  let aux (x, k) = (new_lower_name_to_type_var x, desugar_kind k) in
  List.map aux args

let desugar_value ~current_module imports options (name, is_rec, t) =
  let name = new_lower_name_to_value ~current_module ~allow_underscore:true name in
  desugar_let imports options name is_rec t

let desugar_open ~current_module options = function
  | ParseTree.Source (loc, `UpperName modul) ->
      (loc, Module.create ~current_module modul, modul)
  | ParseTree.Library (loc, `UpperName modul) ->
      (loc, Module.library_create options modul, modul)

let import_aux f ~export ~current_module imports x =
  let aux imports (name, _) = f ~export name current_module imports in
  List.fold_left aux imports x

let import_variants = import_aux Imports.add_variant
let import_sigs = import_aux Imports.add_value

let desugar_instance_name ~current_module f x = function
  | Some name ->
      let x = f name x in
      let name = new_lower_name_to_instance ~current_module ~allow_underscore:false name in
      (Some name, x)
  | None ->
      (None, x)

let rec desugar_kind_from_args = function
  | [] -> KStar
  | (_, k)::ks -> KFun (desugar_kind k, desugar_kind_from_args ks)

let create ~current_module options mimports =
  let rec aux imports = function
    | ParseTree.Value ((name, ParseTree.NonRec, _) as value) :: xs ->
        let value = desugar_value ~current_module imports options value in
        let imports = Imports.add_value ~export:false name current_module imports in
        Value value :: aux imports xs
    | ParseTree.Value ((name, ParseTree.Rec, _) as value) :: xs ->
        let imports = Imports.add_value ~export:false name current_module imports in
        let value = desugar_value ~current_module imports options value in
        Value value :: aux imports xs
    | ParseTree.Type (name, ty) :: xs ->
        let imports = Imports.add_type ~export:false name current_module imports in
        let name = new_upper_name_to_type ~current_module name in
        let ty = desugar_ty imports ty in
        Type (name, ty) :: aux imports xs
    | ParseTree.Foreign (cname, name, ty) :: xs ->
        let cname = desugar_string ~loc:(fst name) cname in
        let imports' = Imports.add_value ~export:false name current_module imports in
        let name = new_lower_name_to_value ~current_module ~allow_underscore:false name in
        let ty = desugar_ty imports ty in
        Foreign (cname, name, ty) :: aux imports' xs
    | ParseTree.AbstractType (name, kind) :: xs ->
        let imports = Imports.add_type ~export:false name current_module imports in
        let name = new_upper_name_to_type ~current_module name in
        let kind = desugar_kind kind in
        Datatype (name, kind, [], []) :: aux imports xs
    | ParseTree.Datatype (name, args, variants) :: xs ->
        let imports = Imports.add_type ~export:false name current_module imports in
        let kind = desugar_kind_from_args args in
        let name = new_upper_name_to_type ~current_module name in
        let args = desugar_variant_args args in
        let imports = import_variants ~export:false ~current_module imports variants in
        let variants = desugar_variants ~current_module imports ~datatype:name ~args variants in
        Datatype (name, kind, args, variants) :: aux imports xs
    | ParseTree.Exception (name, tys) :: xs ->
        if Int.(List.length tys > Config.max_fail_num_args) then
          Err.fail
            ~loc:(fst name)
            "Cannot define an exception with more than %d parameters"
            Config.max_fail_num_args;
        let name' = new_upper_name_to_exn ~current_module name in
        let tys = List.map (desugar_ty imports) tys in
        let imports = Imports.add_exn ~export:false name current_module imports in
        Exception (name', tys) :: aux imports xs
    | ParseTree.Open import :: xs ->
        let (loc, modul', modul) = desugar_open ~current_module options import in
        if not (List.exists (Module.equal modul') mimports) then
          Err.fail ~loc "Module '%s' isn't imported" (Module.to_string modul');
        let imports = Imports.open_module modul imports in
        aux imports xs
    | ParseTree.Class (name, params, sigs) :: xs ->
        let name' = new_upper_name_to_tyclass ~current_module name in
        let params = List.map desugar_t_value params in
        let sigs' = List.map (desugar_sig ~current_module imports) sigs in
        let imports = Imports.add_tyclass ~export:false name current_module imports in
        let imports = import_sigs ~export:false ~current_module imports sigs in
        Class (name', params, sigs') :: aux imports xs
    | ParseTree.Instance (tyclass, name, values) :: xs ->
        let tyclass = desugar_instance imports tyclass in
        let values = List.map (desugar_value ~current_module imports options) values in
        let (name, imports) =
          let aux name = Imports.add_instance ~export:false name current_module in
          desugar_instance_name ~current_module aux imports name
        in
        Instance (tyclass, name, values) :: aux imports xs
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
        let value = desugar_sig ~current_module local_imports signature in
        let imports = Imports.add_value ~export:true name current_module imports in
        aux imports local_imports (IVal value :: acc) xs
    | ParseTree.IAbstractType (name, k) :: xs ->
        let imports = Imports.add_type ~export:true name current_module imports in
        let local_imports = Imports.add_type ~export:false name current_module local_imports in
        let name = new_upper_name_to_type ~current_module name in
        aux imports local_imports (IAbstractType (name, desugar_kind k) :: acc) xs
    | ParseTree.IDatatype (name, args, variants) :: xs ->
        let imports = Imports.add_type ~export:true name current_module imports in
        let local_imports = Imports.add_type ~export:false name current_module local_imports in
        let kind = desugar_kind_from_args args in
        let name = new_upper_name_to_type ~current_module name in
        let args = desugar_variant_args args in
        let imports = import_variants ~export:true ~current_module imports variants in
        let variants = desugar_variants ~current_module local_imports ~datatype:name ~args variants in
        aux imports local_imports (IDatatype (name, kind, args, variants) :: acc) xs
    | ParseTree.ITypeAlias (name, ty) :: xs ->
        let imports = Imports.add_type ~export:true name current_module imports in
        let local_imports' = Imports.add_type ~export:false name current_module local_imports in
        let name = new_upper_name_to_type ~current_module name in
        aux imports local_imports' (ITypeAlias (name, desugar_ty local_imports ty) :: acc) xs
    | ParseTree.IException (name, tys) :: xs ->
        let imports' = Imports.add_exn ~export:true name current_module imports in
        let name = new_upper_name_to_exn ~current_module name in
        aux imports' local_imports (IException (name, List.map (desugar_ty local_imports) tys) :: acc) xs
    | ParseTree.IOpen import :: xs ->
        let (loc, modul', modul) = desugar_open ~current_module options import in
        if not (List.exists (Module.equal modul') mimports) then
          Err.fail ~loc "Module '%s' isn't imported" (Module.to_string modul');
        let local_imports = Imports.open_module modul local_imports in
        aux imports local_imports acc xs
    | ParseTree.IClass (name, params, sigs) :: xs ->
        let imports = Imports.add_tyclass ~export:true name current_module imports in
        let local_imports' = Imports.add_tyclass ~export:false name current_module local_imports in
        let name = new_upper_name_to_tyclass ~current_module name in
        let params = List.map desugar_t_value params in
        let imports = import_sigs ~export:true ~current_module imports sigs in
        let sigs = List.map (desugar_sig ~current_module local_imports) sigs in
        aux imports local_imports' (IClass (name, params, sigs) :: acc) xs
    | ParseTree.IInstance (tyclass, name) :: xs ->
        (* NOTE: No need to add the name in the interfaces *)
        let tyclass = desugar_instance local_imports tyclass in
        let (name, (imports, local_imports)) =
          let aux name (imports, local_imports) =
            let imports = Imports.add_instance ~export:true name current_module imports in
            let local_imports = Imports.add_instance ~export:false name current_module local_imports in
            (imports, local_imports)
          in
          desugar_instance_name ~current_module aux (imports, local_imports) name
        in
        aux imports local_imports (IInstance (tyclass, name) :: acc) xs
    | [] ->
        (imports, acc)
  in
  let (imports, interface) = aux Imports.empty imports [] interface in
  (imports, List.rev interface)
