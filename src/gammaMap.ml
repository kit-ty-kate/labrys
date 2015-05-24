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

module type BASE = sig
  include Map.S
  include module type of Exceptionless
end

module type S = sig
  include BASE

  val union : ('a -> 'a) -> imported:'a t -> 'a t -> 'a t
  val diff : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> string list
  val open_module : Module.t -> 'a t -> 'a t
  val fill_module : key -> 'a t -> key
end

module Utils
         (M : BASE)
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

  let fill_module_aux x self =
    let rec aux x = function
      | [] -> (x, false)
      | (matches, _)::xs ->
          begin match Ident.fill_module ~matches x with
          | (x, true) -> (x, true)
          | (x, false) -> aux x xs
          end
    in
    aux x (M.bindings self)

  let fill_module x self =
    match fill_module_aux x self with
    | (x, true) -> x
    | (_, false) -> assert false
end

module MakeSelf (I : Map.OrderedType) = struct
  include Map.Make(I)
  include Exceptionless
end

module Make (I : module type of Ident.Name) = struct
  module Self = MakeSelf(I)

  include Self
  include Utils(Self)(I)
end

module Value = Make(Ident.Name)

module Types = struct
  include Make(Ident.Type)

  let add k x map =
    if mem k map then
      Error.fail
        ~loc:(Ident.Type.loc k)
        "A module cannot contain several times the type '%s'"
        (Ident.Type.to_string k);
    add k x map
end

module Index = Value

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

  let fill_module x self =
    let rec aux x = function
      | [] -> assert false
      | (_, matches)::xs ->
          begin match Index.fill_module_aux x matches with
          | (x, true) -> x
          | (x, false) -> aux x xs
          end
    in
    aux x (bindings self)
end

module Exn = struct
  include Make(Ident.Exn)

  let add k x map =
    if mem k map then
      Error.fail
        ~loc:(Ident.Exn.loc k)
        "A module cannot contain several times the exception '%s'"
        (Ident.Exn.to_string k);
    add k x map
end
