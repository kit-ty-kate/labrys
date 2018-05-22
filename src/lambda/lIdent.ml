(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type t = < name : string >

let create name = object
  method name = name
end

let equal = Equal.physical
let compare x y = Int.compare (Oo.id x) (Oo.id y)

let to_string x = x#name

type tmp = t

module Map = Map.Make (struct
    type t = tmp
    let compare = compare
  end)

module Set = Set.Make (struct
    type t = tmp
    let compare = compare
  end)

module MSet = CCMultiSet.Make (struct
    type t = tmp
    let compare = compare
  end)
