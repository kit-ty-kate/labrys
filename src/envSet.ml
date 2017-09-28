(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

module Value = Utils.EqSet(Ident.Name)
module MValue = CCMultiSet.Make (Ident.Name)

module IDValue = Utils.EqSet (LIdent)
module MIDValue = CCMultiSet.Make (LIdent)
