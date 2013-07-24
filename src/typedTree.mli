open MonadStdlib

type value = (string * Types.t)

type t =
  | Abs of (Types.t * value * Types.t * t)
  | App of (Types.t * t * t)
  | Val of value

val from_parse_tree :
  value list ->
  Types.ty list ->
  ParseTree.t ->
  (t, [> failure | not_found ]) MonadExn.t
