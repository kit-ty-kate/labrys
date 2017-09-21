(* TODO *)
type t = int

val empty : t
val union : t -> t -> t
val get_untyped_values : t -> LIdent.t EnvMap.Value.t
