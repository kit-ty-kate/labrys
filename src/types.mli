open MonadStdlib

type ty = (string * Backend.ty)

type t =
  | Fun of (t * t)
  | Ty of ty

val to_string : t -> string
val from_parse_tree : ty list -> ParseTree.ty -> (t, [> not_found ]) MonadExn.t
val gamma : ty list
