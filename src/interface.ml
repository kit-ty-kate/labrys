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

type t =
  | Val of (Location.t * ParseTree.name * ParseTree.ty)
  | AbstractType of (Location.t * ParseTree.t_value)
  | Datatype of ParseTree.datatype
  | TypeAlias of (Location.t * ParseTree.t_name * ParseTree.ty)

let compile gamma =
  let rec compile gammaT gamma = function
    | Val (loc, name, ty) :: xs ->
        let ty = TypesBeta.of_parse_tree ~loc gammaT ty in
        let gamma = Gamma.add_value name ty gamma in
        compile gammaT gamma xs
    | AbstractType (loc, (name, k)) :: xs ->
        let gamma = Gamma.add_type ~loc name (Types.Abstract k) gamma in
        let gammaT = GammaMap.Types.add ~loc name (Types.Abstract k) gammaT in
        compile gammaT gamma xs
    | Datatype (loc, name, k, variants) :: xs ->
        let gamma = Gamma.add_type ~loc name (Types.Abstract k) gamma in
        let gammaT = GammaMap.Types.add ~loc name (Types.Abstract k) gammaT in
        let gamma =
          let aux ~datatype gamma i (ParseTree.Variant (loc, name, ty)) =
            let ty = TypesBeta.of_parse_tree ~loc gammaT ty in
            let gamma = Gamma.add_value name ty gamma in
            let gamma = Gamma.add_index name (ty, i) gamma in
            Gamma.append_constr datatype name gamma
          in
          List.fold_lefti (aux ~datatype:name) gamma variants
        in
        compile gammaT gamma xs
    | TypeAlias (loc, name, ty) :: xs ->
        let ty = Types.from_parse_tree ~loc gammaT ty in
        let gamma = Gamma.add_type ~loc name (Types.Alias ty) gamma in
        let gamma = Gamma.add_type ~loc name (Types.Alias ty) gamma in
        let gammaT = GammaMap.Types.add ~loc name (Types.Alias ty) gammaT in
        compile gammaT gamma xs
    | [] ->
        gamma
  in
  compile gamma.Gamma.types Gamma.empty
