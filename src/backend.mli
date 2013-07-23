open MonadStdlib

type t

val from_typed_tree : TypedTree.t -> (t, [> not_found ]) MonadExn.t
val print : t -> (unit, [> sys_error ]) MonadExn.t
