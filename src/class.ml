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

type name = Ident.Name.t
type ty_name = Ident.Type.t

module Instances = Utils.EqSet(struct
    type t = PrivateTypes.t list

    let equal = List.equal PrivateTypes.ty_equal
  end)

(* TODO: Handle contraints *)
type t =
  { params : (ty_name * Kinds.t) list
  ; signature : (name * PrivateTypes.t) list
  ; instances : Instances.t
  }

let create params signature =
  { params
  ; signature
  ; instances = Instances.empty
  }

let remove_module_aliases {params; signature; instances} =
  let params =
    let aux (name, k) = (Ident.Type.remove_aliases name, k) in
    List.map aux params
  in
  let signature =
    let aux (name, ty) =
      (Ident.Name.remove_aliases name, PrivateTypes.ty_remove_module_aliases ty)
    in
    List.map aux signature
  in
  let instances =
    let aux = List.map PrivateTypes.ty_remove_module_aliases in
    Instances.map aux instances
  in
  {params; signature; instances}

let params_equal (name1, k1) (name2, k2) =
  Ident.Type.equal name1 name2 && Kinds.equal k1 k2

let signature_equal (name1, ty1) (name2, ty2) =
  Ident.Name.equal name1 name2
  && PrivateTypes.ty_equal ty1 ty2

let equal a b =
  List.equal params_equal a.params b.params
  && List.equal signature_equal a.signature b.signature
  && Instances.equal a.instances b.instances

let get_params ~loc n self =
  let n' = List.length self.params in
  if Int.Infix.(n <> n') then
    Err.fail
      ~loc
      "Wrong number of parameter. Has %d but expected %d" n n';
  self.params
