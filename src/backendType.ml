type t = (string * string)

let int =
  let ty = "i32" in
  (ty, ty)

let func (ret, _) (param, _) =
  let p = " (" ^ param ^ ")*" in
  (ret ^ p, ret ^ p ^ p)

let to_string = fst
let to_string_call = snd
