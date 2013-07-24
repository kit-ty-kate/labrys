module Exn = MonadExn

open MonadStdlib
open Exn.Ops

type value = (string * Types.t)

type t =
  | Abs of (Types.t * value * Types.t * t)
  | App of (Types.t * t * t)
  | Val of value

let rec get_type = function
  | Abs (_, _, ty, _) -> ty
  | App (ty, _, _) -> ty
  | Val (_, ty) -> ty

let rec from_parse_tree gamma gammaT = function
  | ParseTree.Abs ((name, ty), t) ->
      Types.from_parse_tree gammaT ty >>= fun ty ->
      let v = (name, ty) in
      from_parse_tree (v :: gamma) gammaT t >>= fun x ->
      let ty_x = get_type x in
      Exn.return (Abs (ty_x, v, Types.Fun (ty, ty_x), x))
  | ParseTree.App (f, x) ->
      from_parse_tree gamma gammaT f >>= fun f ->
      from_parse_tree gamma gammaT x >>= fun x ->
      let ty_x = get_type x in
      (match get_type f with
        | Types.Fun (ty, res) when Unsafe.(ty = ty_x) ->
            Exn.return (App (res, f, x))
        | Types.Fun (ty, _) ->
            failwith
              ("Error: This expression has type "
               ^ Types.to_string ty_x
               ^ " but an expression was expected of type "
               ^ Types.to_string ty
              )
        | Types.Ty _ as ty ->
            failwith
              ("Typechecker: Can't apply to a non-function type ("
               ^ Types.to_string ty
               ^ " to "
               ^ Types.to_string ty_x
               ^ ")"
              )
      )
  | ParseTree.Val name ->
      List.find (fun (name', _) -> Unsafe.(name = name')) gamma >>= fun x ->
      Exn.return (Val x)
