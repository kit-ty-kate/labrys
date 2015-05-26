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

open Monomorphic_containers

module type S = sig
  include Utils.EQMAP

  val union : ('a -> 'a) -> imported:'a t -> 'a t -> 'a t
  val diff : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> string list
  val open_module : Module.t -> 'a t -> 'a t
end

module Aux
         (M : Utils.EQMAP)
         (Ident : module type of Ident.Name with type t = M.key) = struct
  let union f ~imported b =
    let aux k x = M.add (Ident.remove_aliases k) (f x) in
    M.fold aux imported b

  let diff ~eq a b =
    let aux k x acc =
      match M.find k b with
      | Some y when eq x y -> acc
      | Some _ -> Ident.to_string k :: acc
      | None -> Ident.to_string k :: acc
    in
    M.fold aux a []

  let open_module modul self =
    let aux k = M.add (Ident.open_module modul k) in
    M.fold aux self M.empty

  let fill_module_aux k self =
    let rec aux = function
      | [] -> None
      | (matches, x)::xs ->
          begin match Ident.fill_module ~matches k with
          | Some k -> Some (k, x)
          | None -> aux xs
          end
    in
    aux (M.bindings self)
end

module Make (I : module type of Ident.Name) = struct
  module Self = Utils.EqMap(I)

  include Self
  include Aux(Self)(I)
end

module Value = struct
  include Make(Ident.Name)

  let fill_module k self =
    match fill_module_aux k self with
    | Some x ->
        x
    | None ->
        Err.fail
          ~loc:(Ident.Name.loc k)
          "The value '%s' was not found in Γ"
          (Ident.Name.to_string k)
end

module Types = struct
  include Make(Ident.Type)

  let add k x map =
    if mem k map then
      Err.fail
        ~loc:(Ident.Type.loc k)
        "A module cannot contain several times the type '%s'"
        (Ident.Type.to_string k);
    add k x map

  let fill_module k self =
    match fill_module_aux k self with
    | Some x ->
        x
    | None ->
        Err.fail
          ~loc:(Ident.Type.loc k)
          "The type '%s' was not found in Γ"
          (Ident.Type.to_string k)
end

module Index = struct
  include Make(Ident.Name)

  let fill_module ~head_ty k self =
    match fill_module_aux k self with
    | Some x ->
        x
    | None ->
        Err.fail
          ~loc:(Ident.Name.loc k)
          "Constructor '%s' not found in type '%s'"
          (Ident.Name.to_string k)
          (Ident.Type.to_string head_ty)

end

module Constr = struct
  include Make(Ident.Type)

  let open_module modul self =
    let aux k idx =
      add (Ident.Type.open_module modul k) (Index.open_module modul idx)
    in
    fold aux self empty

  let add k k2 x map =
    match find k map with
    | None -> add k (Index.singleton k2 x) map
    | Some xs -> add k (Index.add k2 x xs) map
end

module Exn = struct
  include Make(Ident.Exn)

  let add k x map =
    if mem k map then
      Err.fail
        ~loc:(Ident.Exn.loc k)
        "A module cannot contain several times the exception '%s'"
        (Ident.Exn.to_string k);
    add k x map

  let fill_module k self =
    match fill_module_aux k self with
    | Some x -> x
    | None ->
        Err.fail
          ~loc:(Ident.Exn.loc k)
          "The exception '%s' is not defined in Γ"
          (Ident.Exn.to_string k)
end
