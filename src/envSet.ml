(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

module Value = struct
  include Utils.EqSet(Ident.Name)
end

module TypeVar = EnvMap.TypeVar.Set

module MValue = CCMultiSet.Make (Ident.Name)

module IDValue = Utils.EqSet (LIdent)
module MIDValue = CCMultiSet.Make (LIdent)
