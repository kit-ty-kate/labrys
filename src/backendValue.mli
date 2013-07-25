type t

val global : string -> t
val local : string -> t

val to_string : t -> string
val apply : (t -> 'a) -> (t -> 'a) -> t -> 'a
