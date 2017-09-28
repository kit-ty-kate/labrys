(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

let empty = ()
let union x y = assert false
let diff x y = assert false
let get_untyped_values env = assert false

type add = TypedEnv.env -> TypedEnv.env

let add_toplevel_value name ty env =
  assert false

let add_abstract_type name k env =
  assert false

let add_datatype name k args variants env =
  assert false

let add_type_alias name ty env =
  assert false

let add_exception name args env =
  assert false

let add_class name args sigs env =
  assert false

let add_instance instance name env =
  assert false
