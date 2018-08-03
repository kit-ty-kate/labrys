(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

exception ParseError of string

module Make (Filename : sig val get : string end) : sig
  val parse_impl : unit -> ParseTree.imports * ParseTree.top list

  val parse_intf : unit -> ParseTree.imports * ParseTree.interface list
end
