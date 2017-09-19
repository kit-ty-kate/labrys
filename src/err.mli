(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type t

exception Exn of t

val fail : loc:Location.t -> ('a, unit, string, 'b) format4 -> 'a
val fail_doc : loc:Location.t -> PPrint.document -> 'a
val fail_module : ('a, unit, string, 'b) format4 -> 'a

val dump : t -> string
