(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type t = < name : string >

let create name = object
  method name = name
end

let equal = (==)
let compare x y = Int.compare (Oo.id x) (Oo.id y)

let to_string x = x#name

type tmp = t

module Map = Utils.EqMap (struct
    type t = tmp
    let equal = equal
  end)

module Set = Utils.EqSet (struct
    type t = tmp
    let equal = equal
  end)

module MSet = CCMultiSet.Make (struct
    type t = tmp
    let compare = compare
  end)
