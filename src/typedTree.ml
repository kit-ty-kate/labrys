module Exn = MonadExn

open MonadStdlib
open Exn.Ops

type value = (string * Types.t)

type t =
  | Abs of (value * Types.t * t)
  | App of (Types.t * t * t)
  | Val of value

let rec get_type = function
  | Abs (_, ty, _) -> ty
  | App (ty, _, _) -> ty
  | Val (_, ty) -> ty

let rec from_parse_tree gamma gammaT = function
  | ParseTree.Abs ((name, ty), t) ->
      Types.from_parse_tree gammaT ty >>= fun ty ->
      let v = (name, ty) in
      from_parse_tree (v :: gamma) gammaT t >>= fun x ->
      Exn.return (Abs (v, Types.Fun (ty, get_type x), x))
  | ParseTree.App (f, x) ->
      from_parse_tree gamma gammaT f >>= fun f ->
      from_parse_tree gamma gammaT x >>= fun x ->
      let ty_x = get_type x in
      (match get_type f with
        | Types.Fun (ty, _) when Unsafe.(ty = ty_x) ->
            Exn.return (App (ty, f, x))
        | Types.Fun (ty, _) ->
            failwith
              ("Error: This expression has type "
               ^ Types.to_string ty_x
               ^ " but an expression was expected of type "
               ^ Types.to_string ty
              )
        | Types.Ty _ ->
            failwith "Typechecker: Can't apply to a non-function type"
      )
  | ParseTree.Val name ->
      List.find (fun (name', _) -> Unsafe.(name = name')) gamma >>= fun x ->
      Exn.return (Val x)
