(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type name = Ident.Name.t
type variant_name = Ident.Variant.t

type var = private
  | VLeaf
  | VNode of (int * var)

type mconstr

type 'a t = (mconstr * 'a) list

type code_index = int

type pattern = private
  | Constr of (var * (variant_name * Ident.Type.t) * pattern list)
  | Any of (var * (name * Ident.Type.t))

type matrix = (pattern list * code_index) list

val create :
  Gamma.t ->
  Types.t ->
  DesugaredTree.pattern ->
  (mconstr * Gamma.t)

val split : 'a t -> (matrix * ((var * name) list * 'a) list)
