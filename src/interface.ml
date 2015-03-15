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

open InterfaceTree

let compile gamma =
  let rec compile gammaT gamma = function
    | (_loc, Val (name, ty)) :: xs ->
        let ty = Types.of_parse_tree gammaT ty in
        let gamma = Gamma.add_value name ty gamma in
        compile gammaT gamma xs
    | (loc, AbstractType (name, k)) :: xs ->
        let gamma = Gamma.add_type ~loc name (`Abstract k) gamma in
        let gammaT = GammaMap.Types.add ~loc name (`Abstract k) gammaT in
        compile gammaT gamma xs
    | (loc, Datatype (name, k, variants)) :: xs ->
        let gamma = Gamma.add_type ~loc name (`Abstract k) gamma in
        let gammaT = GammaMap.Types.add ~loc name (`Abstract k) gammaT in
        let gamma =
          let aux ~datatype gamma i (UnsugaredTree.Variant (loc, name, ty)) =
            let ty = Types.of_parse_tree gammaT ty in
            if Types.check_if_returns_type ~datatype ty then
              let gamma = Gamma.add_value name ty gamma in
              Gamma.add_constr datatype name (ty, i) gamma
            else
              Types.Error.fail_return_type ~loc name
          in
          List.fold_lefti (aux ~datatype:name) gamma variants
        in
        compile gammaT gamma xs
    | (loc, TypeAlias (name, ty)) :: xs ->
        let ty = Types.of_parse_tree_kind gammaT ty in
        let gamma = Gamma.add_type ~loc name (`Alias ty) gamma in
        let gammaT = GammaMap.Types.add ~loc name (`Alias ty) gammaT in
        compile gammaT gamma xs
    | (loc, Exception (name, args)) :: xs ->
        let args = List.map (Types.of_parse_tree gammaT) args in
        let gamma = Gamma.add_exception ~loc name args gamma in
        compile gammaT gamma xs
    | [] ->
        gamma
  in
  compile gamma.Gamma.types Gamma.empty
