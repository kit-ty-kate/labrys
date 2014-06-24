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

(* TODO: Remove duplicate *)

module Value = struct
  include Map.Make(Ident.Name)
  include Exceptionless

  let union (modul, a) b =
    let aux k = add (Ident.Name.prepend modul k) in
    fold aux a b

  let diff ~eq a b =
    let aux k x acc =
      match find k b with
      | Some y when eq x y -> acc
      | Some _ -> Ident.Name.to_string k :: acc
      | None -> Ident.Name.to_string k :: acc
    in
    fold aux a []
end

module Types = struct
  include Map.Make(Ident.Type)
  include Exceptionless

  let union (modul, a) b =
    let aux k = add (Ident.Type.prepend modul k) in
    fold aux a b

  let diff ~eq a b =
    let aux k x acc =
      match find k b with
      | Some y when eq x y -> acc
      | Some _ -> Ident.Type.to_string k :: acc
      | None -> Ident.Type.to_string k :: acc
    in
    fold aux a []

  let add ~loc k x map =
    if mem k map then
      Error.fail
        ~loc
        "A module cannot contain several times the type '%s'"
        (Ident.Type.to_string k);
    add k x map
end

module Index = Value

module Constr = struct
  include Map.Make(Ident.Type)
  include Exceptionless

  let union (modul, a) b =
    let aux k = add (Ident.Type.prepend modul k) in
    fold aux a b

  let diff ~eq a b =
    let aux k x acc =
      match find k b with
      | Some y when eq x y -> acc
      | Some _ -> Ident.Type.to_string k :: acc
      | None -> Ident.Type.to_string k :: acc
    in
    fold aux a []

  let add k k2 x map =
    match find k map with
    | None -> add k (Index.singleton k2 x) map
    | Some xs -> add k (Index.add k2 x xs) map
end
