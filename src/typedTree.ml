module Exn = MonadExn

open MonadStdlib
open Exn.Ops

type t =
  | Abs of (Value.t * Types.t * t)
  | App of (Types.t * t * t)
  | Val of Value.t

let rec get_type = function
  | Abs (_, ty, _) -> ty
  | App (ty, _, _) -> ty
  | Val (_, ty) -> ty

let rec from_parse_tree gamma = function
  | ParseTree.Abs (v, t) ->
      from_parse_tree (v :: gamma) t >>= fun x ->
      Exn.return (Abs (v, Types.Fun (snd v, get_type x), x))
  | ParseTree.App (f, x) ->
      from_parse_tree gamma f >>= fun f ->
      from_parse_tree gamma x >>= fun x ->
      (match get_type f with
        | Types.Fun (ty, _) when Unsafe.(ty = get_type x) ->
            Exn.return (App (ty, f, x))
        | Types.Fun _ ->
            failwith "Typechecker: Argument type doesn't match"
        | Types.Ty _ ->
            failwith "Typechecker: Can't apply to a non-function type"
      )
  | ParseTree.Val name ->
      List.find (fun (name', _) -> Unsafe.(name = name')) gamma >>= fun x ->
      Exn.return (Val x)
