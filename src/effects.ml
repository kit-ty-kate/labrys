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

let fmt = Printf.sprintf

module Exn_set = Set.Make(Ident.Exn)
module Variables = Map.Make(Ident.Eff)

type t = Exn_set.t Variables.t

let empty = Variables.empty
let is_empty = Variables.is_empty

let var_eq exn_eq list_eq x y =
  let aux k exns =
    let mem k =
      match Variables.Exceptionless.find k y with
      | Some exns' -> exn_eq exns exns'
      | None -> false
    in
    match List.find (fun (k', _) -> Ident.Eff.equal k k') list_eq with
    | Some (_, k) -> mem k
    | None -> mem k
  in
  Variables.for_all aux x

let equal list_eq x y =
  Int.(Variables.cardinal x = Variables.cardinal y)
  && var_eq Exn_set.equal list_eq x y

let is_subset_of list_eq x y =
  var_eq Exn_set.subset list_eq x y

let has_io x =
  let x = Variables.remove Builtins.exn x in
  not (Variables.is_empty x)

let add ~loc gammaE (name, args) self =
  match GammaMap.Eff.find name gammaE with
  | Some has_args ->
      if has_args && List.is_empty args then
        Error.fail
          ~loc
          "The '%s' effect must have at least one argument"
          (Ident.Eff.to_string name);
      if not has_args && not (List.is_empty args) then
        Error.fail
          ~loc
          "The '%s' effect doesn't have any arguments"
          (Ident.Eff.to_string name);
      let args = Exn_set.of_list args in
      Variables.modify_def args name (Exn_set.union args) self
  | None ->
      Error.fail ~loc "Unknown effect '%s'" (Ident.Eff.to_string name)

let add_exn x self =
  let def = Exn_set.singleton x in
  Variables.modify_def def Builtins.exn (Exn_set.add x) self

let union x y =
  let aux name args self =
    if Ident.Eff.equal name Builtins.exn then
      Variables.modify_def args name (Exn_set.union args) self
    else if not (Exn_set.is_empty args) then
      assert false
    else
      Variables.add name args self
  in
  Variables.fold aux y x

let union3 x y z =
  union (union x y) z

let remove_exn ~loc x self =
  let fail () =
    Error.fail
      ~loc
      "Useless case. The exception '%s' is not included in the handled \
       expression"
      (Ident.Exn.to_string x)
  in
  match Variables.Exceptionless.find Builtins.exn self with
  | Some exns ->
      if not (Exn_set.mem x exns) then
        fail ();
      let exns = Exn_set.remove x exns in
      if Exn_set.is_empty exns then
        Variables.remove Builtins.exn self
      else
        Variables.add Builtins.exn exns self
  | None ->
      fail ()

let to_string self =
  let aux name args acc =
    let aux name acc = acc @ [Ident.Exn.to_string name] in
    let args = String.concat " | " (Exn_set.fold aux args []) in
    acc @ [Ident.Eff.to_string name ^ args]
  in
  fmt "[%s]" (String.concat ", " (Variables.fold aux self []))

let of_list ~loc gammaE l =
  let aux acc ty = add ~loc gammaE ty acc in
  List.fold_left aux empty l

let replace ~from ~eff self =
  let aux name x self =
    if Ident.Eff.equal name from then begin
      if not (Exn_set.is_empty x) then
        assert false;
      union self eff
    end else
      Variables.add name x self
  in
  Variables.fold aux self empty
