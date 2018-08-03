(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

type position = {pos_lnum : int; pos_cnum : int}

type t = {loc_start : position; loc_end : position; filename : string}
