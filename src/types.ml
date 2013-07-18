type t =
  | Fun of (t * t)
  | Ty of string
