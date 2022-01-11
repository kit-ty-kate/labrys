(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

val version : string
val git_version : string
val lib : string Lazy.t

val parse_dir : string -> (string, [`Msg of string]) result
