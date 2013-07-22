type ty =
  | Fun of (ty * ty)
  | Ty of string

type value = (string * ty)

type t =
  | Abs of (value * t)
  | App of (t * t)
  | Val of string
