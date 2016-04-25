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

type name = Ident.Name.t
type tyvar_name = Ident.TypeVar.t

module Instances = PrivateTypes.Instances

(* TODO: Handle contraints *)
type t = PrivateTypes.class_t =
  { params : (tyvar_name * Kinds.t) list
  ; signature : (name * PrivateTypes.t) list
  ; instances : name Instances.t
  }

let create params signature =
  { params
  ; signature
  ; instances = Instances.empty
  }

let equal = PrivateTypes.class_equal

let get_params ~loc f gamma tyvars args self =
  let gamma =
    GammaMap.TypeVar.fold Gamma.add_type_var tyvars gamma
  in
  try
    let aux (gamma, args) (_, k) ty =
      let loc = fst ty in
      let (ty, k') = f gamma ty in
      if not (Kinds.equal k k') then
        Kinds.Err.fail ~loc ~has:k' ~expected:k;
      (gamma, ty :: args)
    in
    let (gamma, args) =
      List.fold_left2 aux (gamma, []) self.params args
    in
    (gamma, List.rev args)
  with
  | Invalid_argument _ ->
      Err.fail
        ~loc
        "Wrong number of parameter. Has %d but expected %d"
        (List.length args)
        (List.length self.params)

let get_instance_name ~loc ~tyclass tys self =
  match Instances.find tys self.instances with
  | Some x -> x
  | None ->
      Err.fail
        ~loc
        "No instance found for '%s %s'"
        (Ident.TyClass.to_string tyclass)
        (String.concat " " (List.map PrivateTypes.ty_to_string tys))

(* TODO: Improve loc *)
let add_instance ~tyclass ~current_module tys self =
  let aux (ty, k1) (_, k2) =
    if not (Kinds.equal k1 k2) then
      Err.fail
        ~loc:(Ident.TyClass.loc tyclass)
        "Kinds doesn't match. Has '%s' but expected '%s'"
        (Kinds.to_string k1)
        (Kinds.to_string k2);
    ty
  in
  let tys = List.map2 aux tys self.params in
  let name =
    let names = List.map PrivateTypes.ty_to_string tys in
    let names = Ident.TyClass.to_string tyclass :: names in
    let name = "$instance$" ^ String.concat "$" names in
    Ident.Name.create ~loc:Builtins.unknown_loc current_module name
  in
  let instances = Instances.add tys name self.instances in
  (name, tys, {self with instances})

let get_values ~loc tys values self =
  let aux (acc, n) ((name1, is_rec, t), ty1) (name2, ty2) =
    if not (Ident.Name.equal name1 name2) then
      Err.fail
        ~loc:(Ident.Name.loc name1)
        "Name missmatch. Has '%s' but expected '%s'"
        (Ident.Name.to_string name1)
        (Ident.Name.to_string name2);
    let ty2 =
      let aux ty2 (from, _) ty = PrivateTypes.ty_replace ~from ~ty ty2 in
      try
        List.fold_left2 aux ty2 self.params tys
      with
      | Invalid_argument _ -> assert false
    in
    if not (PrivateTypes.ty_equal ty1 ty2) then
      Err.fail
        ~loc:(Ident.Name.loc name1)
        "Type missmatch. Has '%s' but expected '%s'"
        (PrivateTypes.ty_to_string ty1)
        (PrivateTypes.ty_to_string ty2);
    ((name1, is_rec, t) :: acc, succ n)
  in
  try
    let (values, _) = List.fold_left2 aux ([], 0) values self.signature in
    List.rev values
  with
  | Invalid_argument _ ->
      Err.fail ~loc "Signatures missmatch"
