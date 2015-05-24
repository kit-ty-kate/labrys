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

let fmt = Printf.sprintf

module Exn_set = Set.Make(Ident.Exn)
module Variables = Set.Make(Ident.Eff)

type t =
  { variables : Variables.t
  ; exns : Exn_set.t
  }

let empty =
  { variables = Variables.empty
  ; exns = Exn_set.empty
  }

let is_empty {variables; exns} =
  Variables.is_empty variables
  && Exn_set.is_empty exns

let var_eq list_eq x y =
  let aux k =
    let mem k = Variables.mem k y in
    match List.find (fun (k', _) -> Ident.Eff.equal k k') list_eq with
    | Some (_, k) -> mem k
    | None -> mem k
  in
  Variables.for_all aux x

let equal list_eq x y =
  Int.(Variables.cardinal x.variables = Variables.cardinal y.variables)
  && var_eq list_eq x.variables y.variables
  && Exn_set.equal x.exns y.exns

let is_subset_of list_eq x y =
  var_eq list_eq x.variables y.variables
  && Exn_set.subset x.exns y.exns

let has_io {variables; exns = _} =
  not (Variables.is_empty variables)

let add gammaExn gammaE (name, exns) self =
  let exns =
    List.map (fun x -> fst (GammaMap.Exn.fill_module x gammaExn)) exns
  in
  if GammaSet.Eff.mem name gammaE then begin
    let has_args = Ident.Eff.equal name Builtins.exn in
    if has_args && List.is_empty exns then
      Error.fail
        ~loc:(Ident.Eff.loc name)
        "The '%s' effect must have at least one argument"
        (Ident.Eff.to_string name);
    if not has_args && not (List.is_empty exns) then
      Error.fail
        ~loc:(Ident.Eff.loc name)
        "The '%s' effect doesn't have any arguments"
        (Ident.Eff.to_string name);
    let exns = Exn_set.of_list exns in
    let exns = Exn_set.union exns self.exns in
    let variables =
      if has_args then
        self.variables
      else
        Variables.add name self.variables
    in
    {variables; exns}
  end else
    Error.fail
      ~loc:(Ident.Eff.loc name)
      "Unknown effect '%s'"
      (Ident.Eff.to_string name)

let add_exn x self =
  {self with exns = Exn_set.add x self.exns}

let union x y =
  let variables = Variables.union x.variables y.variables in
  let exns = Exn_set.union x.exns y.exns in
  {variables; exns}

let union3 x y z =
  union (union x y) z

let remove_exn x self =
  if not (Exn_set.mem x self.exns) then
    Error.fail
      ~loc:(Ident.Exn.loc x)
      "Useless case. The exception '%s' is not included in the handled \
       expression"
      (Ident.Exn.to_string x);
  let exns = Exn_set.remove x self.exns in
  {self with exns}

let to_string self =
  let aux name acc = Ident.Eff.to_string name :: acc in
  let exns =
    if Exn_set.is_empty self.exns then
      []
    else
      let aux name acc = Ident.Exn.to_string name :: acc in
      [fmt "Exn [%s]" (String.concat " | " (Exn_set.fold aux self.exns []))]
  in
  fmt "[%s]" (String.concat ", " (exns @ Variables.fold aux self.variables []))

let of_list gammaExn gammaE (_, l) =
  let aux acc ty = add gammaExn gammaE ty acc in
  List.fold_left aux empty l

let replace ~from ~eff self =
  let aux name self =
    if Ident.Eff.equal name from then begin
      union self eff
    end else
      {self with variables = Variables.add name self.variables}
  in
  Variables.fold aux self.variables empty

let remove_module_aliases self =
  let exns = Exn_set.map Ident.Exn.remove_aliases self.exns in
  {self with exns}
