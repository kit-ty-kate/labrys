type t =
  | Global of string
  | Local of string

let global x = Global ("@" ^ x)
let local x = Local ("%" ^ x)

let to_string = function
  | Global x -> x
  | Local x -> x

let apply f g = function
  | Global _ as x -> f x
  | Local _ as x -> g x
