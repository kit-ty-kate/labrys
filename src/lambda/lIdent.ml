(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

(** NOTE: the unused id field is needed to avoid optimization by
    the ocaml compiler *)
type t = {name : string; id : unit ref}

let create name = {name; id = ref ()}

let raw_ptr (x : t) =
  Nativeint.shift_left (Nativeint.of_int (Obj.magic x)) 1

let equal = (==)
let compare x y = Nativeint.compare (raw_ptr x) (raw_ptr y)

let to_string x = x.name

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
