(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open Containers
open Monomorphic.None

module type S = sig
  include Utils.EQMAP

  val diff : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> string list
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
end

module Aux
         (M : Utils.EQMAP)
         (Ident : module type of Ident.Name with type t = M.key) = struct
  let diff ~eq a b =
    let aux k x acc =
      match M.find k b with
      | Some y when eq x y -> acc
      | Some _ -> Ident.to_string k :: acc
      | None -> Ident.to_string k :: acc
    in
    M.fold aux a []
end

module Make (I : module type of Ident.Name) = struct
  module Self = Utils.EqMap(I)

  include Self
  include Aux(Self)(I)

  let find_opt = find
end

module Value = struct
  include Make(Ident.Name)

  let fail k =
    Err.fail
      ~loc:(Ident.Name.loc k)
      "The value '%s' was not found in Γ"
      (Ident.Name.to_string k)

  let find k self =
    match find k self with
    | Some x -> x
    | None -> fail k
end

module Variant = struct
  include Make(Ident.Variant)

  let fail k =
    Err.fail
      ~loc:(Ident.Variant.loc k)
      "The variant '%s' was not found in Γ"
      (Ident.Variant.to_string k)

  let find k self =
    match find k self with
    | Some x -> x
    | None -> fail k
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

  let fail k =
    Err.fail
      ~loc:(Ident.Type.loc k)
      "The type '%s' was not found in Γ"
      (Ident.Type.to_string k)

  let find k self =
    match find k self with
    | Some x -> x
    | None -> fail k
end

module TypeVar = struct
  include Make(Ident.TypeVar)

  let fail k =
    Err.fail
      ~loc:(Ident.TypeVar.loc k)
      "The type variable '%s' was not found in Γ"
      (Ident.TypeVar.to_string k)

  let find k self =
    match find k self with
    | Some x -> x
    | None -> fail k
end

module Index = struct
  include Make(Ident.Variant)

  let fail ~head_ty k =
    Err.fail
      ~loc:(Ident.Variant.loc k)
      "Constructor '%s' not found in type '%s'"
      (Ident.Variant.to_string k)
      (Ident.Type.to_string head_ty)

  let find ~head_ty k self =
    match find k self with
    | Some x -> x
    | None -> fail ~head_ty k
end

module Constr = struct
  include Make(Ident.Type)

  let add k k2 args x map =
    match find k map with
    | None -> add k (args, Index.singleton k2 x) map
    | Some (args, xs) -> add k (args, Index.add k2 x xs) map
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

  let fail k =
    Err.fail
      ~loc:(Ident.Exn.loc k)
      "The exception '%s' is not defined in Γ"
      (Ident.Exn.to_string k)

  let find k self =
    match find k self with
    | Some x -> x
    | None -> fail k
end

module TyClass = struct
  include Make(Ident.TyClass)

  let replace = add

  let add k x map =
    if mem k map then
      Err.fail
        ~loc:(Ident.TyClass.loc k)
        "A module cannot contain several times the same class '%s'"
        (Ident.TyClass.to_string k);
    add k x map

  let fail k =
    Err.fail
      ~loc:(Ident.TyClass.loc k)
      "The class '%s' is not defined in Γ"
      (Ident.TyClass.to_string k)

  let find k self =
    match find k self with
    | Some x -> x
    | None -> fail k
end

module Instance = struct
  include Make(Ident.Instance)

  let fail k =
    Err.fail
      ~loc:(Ident.Instance.loc k)
      "The named instance '%s' is not defined in Γ"
      (Ident.Instance.to_string k)

  let find k self =
    match find k self with
    | Some x -> x
    | None -> fail k
end

module TyClassParams = struct
  include Make(Ident.Type)
end
