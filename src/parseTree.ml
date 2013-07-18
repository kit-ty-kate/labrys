type t =
  | Abs of (Value.t * t)
  | App of (t * t)
  | Val of string
