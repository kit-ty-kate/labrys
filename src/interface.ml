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

open InterfaceTree

let compile gamma =
  let rec compile gammaT gammaExn gammaE gamma = function
    | Val (name, ty) :: xs ->
        let ty = Types.of_parse_tree ~pure_arrow:`Partial gammaT gammaExn gammaE ty in
        let gamma = Gamma.add_value name ty gamma in
        compile gammaT gammaExn gammaE gamma xs
    | AbstractType (name, k) :: xs ->
        let gamma = Gamma.add_type name (Types.Abstract k) gamma in
        let gammaT = GammaMap.Types.add name (Types.Abstract k) gammaT in
        compile gammaT gammaExn gammaE gamma xs
    | Datatype (name, k, variants) :: xs ->
        let gamma = Gamma.add_type name (Types.Abstract k) gamma in
        let gammaT = GammaMap.Types.add name (Types.Abstract k) gammaT in
        let gamma =
          let aux ~datatype gamma i (UnsugaredTree.Variant (name, ty)) =
            let ty = Types.of_parse_tree ~pure_arrow:`Partial gammaT gammaExn gammaE ty in
            Types.check_if_returns_type ~name ~datatype ty;
            let gamma = Gamma.add_value name ty gamma in
            Gamma.add_constr datatype name (ty, i) gamma
          in
          List.Idx.foldi (aux ~datatype:name) gamma variants
        in
        compile gammaT gammaExn gammaE gamma xs
    | TypeAlias (name, ty) :: xs ->
        let ty =
          Types.of_parse_tree_kind ~pure_arrow:`Forbid gammaT gammaExn gammaE ty
        in
        let gamma = Gamma.add_type name (Types.Alias ty) gamma in
        let gammaT = GammaMap.Types.add name (Types.Alias ty) gammaT in
        compile gammaT gammaExn gammaE gamma xs
    | Exception (name, args) :: xs ->
        let args =
          List.map (Types.of_parse_tree ~pure_arrow:`Forbid gammaT gammaExn gammaE) args
        in
        let gamma = Gamma.add_exception name args gamma in
        let gammaExn = GammaMap.Exn.add name args gammaExn in
        compile gammaT gammaExn gammaE gamma xs
    | Open modul :: xs ->
        let gammaT = GammaMap.Types.open_module modul gammaT in
        let gammaExn = GammaMap.Exn.open_module modul gammaExn in
        compile gammaT gammaExn gammaE gamma xs
    | [] ->
        gamma
  in
  compile gamma.Gamma.types gamma.Gamma.exceptions gamma.Gamma.effects Gamma.empty
