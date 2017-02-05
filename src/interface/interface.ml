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

open InterfaceTree

let compile ~current_module options gamma =
  let rec compile ~gamma ~local_gamma = function
    | Val (name, ty) :: xs ->
        let ty = Types.of_parse_tree ~pure_arrow:`Partial options local_gamma ty in
        let gamma = Gamma.add_value name ty gamma in
        compile ~gamma ~local_gamma xs
    | AbstractType (name, k) :: xs ->
        let gamma = Gamma.add_type name (Types.Abstract k) gamma in
        let local_gamma = Gamma.add_type name (Types.Abstract k) local_gamma in
        compile ~gamma ~local_gamma xs
    | Datatype (name, k, args, variants) :: xs ->
        let ty_args = List.map fst args in
        let gamma = Gamma.add_type name (Types.Abstract k) gamma in
        let local_gamma = Gamma.add_type name (Types.Abstract k) local_gamma in
        let gamma =
          let local_gamma' = List.fold_left (fun local_gamma (name, k) -> Gamma.add_type_var name k local_gamma) local_gamma args in
          let aux ~datatype gamma i (DesugaredTree.Variant (name, tys, ty)) =
            let tys = List.map (Types.of_parse_tree ~pure_arrow:`Partial options local_gamma') tys in
            let ty = Types.of_parse_tree ~pure_arrow:`Partial options local_gamma ty in
            let gamma = Gamma.add_variant name (i, ty, List.length tys) gamma in
            Gamma.add_constr datatype name ty_args (tys, i) gamma
          in
          List.Idx.foldi (aux ~datatype:name) gamma variants
        in
        compile ~gamma ~local_gamma xs
    | TypeAlias (name, ty) :: xs ->
        let ty = Types.of_parse_tree_kind ~pure_arrow:`Forbid options local_gamma ty in
        let gamma = Gamma.add_type name (Types.Alias ty) gamma in
        let local_gamma = Gamma.add_type name (Types.Alias ty) local_gamma in
        compile ~gamma ~local_gamma xs
    | Exception (name, args) :: xs ->
        let args =
          List.map (Types.of_parse_tree ~pure_arrow:`Forbid options local_gamma) args
        in
        let gamma = Gamma.add_exception name args gamma in
        let local_gamma = Gamma.add_exception name args local_gamma in
        compile ~gamma ~local_gamma xs
    | Class (name, params, sigs) :: xs ->
        let sigs =
          let local_gamma =
            let aux local_gamma (name, k) =
              Gamma.add_type_var name k local_gamma
            in
            List.fold_left aux local_gamma params
          in
          let aux (name, ty) =
            (name, Types.of_parse_tree ~pure_arrow:`Forbid options local_gamma ty)
          in
          List.map aux sigs
        in
        let tyclass = Class.create params sigs in
        let gamma = Gamma.add_tyclass name tyclass gamma in
        let local_gamma = Gamma.add_tyclass name tyclass local_gamma in
        let gamma =
          let aux gamma (name_sig, ty) =
            let ty = Types.tyclass_wrap name params ty in
            Gamma.add_value name_sig ty gamma
          in
          List.fold_left aux gamma sigs
        in
        compile ~gamma ~local_gamma xs
    | Instance ((tyclass, tys), name) :: xs ->
        let tyclass' = GammaMap.TyClass.find tyclass local_gamma.Gamma.tyclasses in
        let tys = List.map (Types.of_parse_tree_kind ~pure_arrow:`Forbid options local_gamma) tys in
        let (_, tys, tyclass') = Class.add_instance ~tyclass ~current_module tys tyclass' in
        let gamma = match name with
          | Some name ->
              Gamma.add_named_instance name (tyclass, tys) gamma
          | None ->
              gamma
        in
        let gamma = Gamma.replace_tyclass tyclass tyclass' gamma in
        compile ~gamma ~local_gamma xs
    | [] ->
        gamma
  in
  compile ~gamma:Gamma.empty ~local_gamma:gamma
