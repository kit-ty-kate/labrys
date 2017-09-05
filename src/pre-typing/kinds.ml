(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type t =
  | Star
  | Eff
  | KFun of (t * t)

let rec from_list = function
  | [] -> Star
  | k::ks -> KFun (k, from_list ks)

let rec to_string = function
  | Star -> "*"
  | Eff -> "φ"
  | KFun (p, r) -> to_string p ^ " -> " ^ to_string r

let rec equal x y = match x, y with
  | Eff, Eff
  | Star, Star -> true
  | KFun (p1, r1), KFun (p2, r2) -> equal p1 p2 && equal r1 r2
  | Star, _
  | KFun _, _
  | Eff, _ -> false

let not_star = function
  | Star -> false
  | KFun _ | Eff -> true

let is_effect = function
  | Eff -> true
  | Star | KFun _ -> false

module Err = struct
  let fail ~loc ~has ~expected =
    Err.fail
      ~loc
      "Error: This type has kind '%s' but a \
       type was expected of kind '%s'"
      (to_string has)
      (to_string expected)
end
