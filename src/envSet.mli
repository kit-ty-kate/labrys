(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

module Value : Utils.EQSET with type elt = Ident.Name.t

module TypeVar = EnvMap.TypeVar.Set

module MValue : CCMultiSet.S with type elt = Ident.Name.t

module IDValue : Utils.EQSET with type elt = LIdent.t
module MIDValue : CCMultiSet.S with type elt = LIdent.t
