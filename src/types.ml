type t =
  | Fun of (t * t)
  | Ty of string

let rec to_string = function
  | Fun (Ty x, ret) -> x ^ " -> " ^ to_string ret
  | Fun (x, ret) -> "(" ^ to_string x ^ ") -> " ^ to_string ret
  | Ty x -> x
