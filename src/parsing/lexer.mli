(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

module Make (Filename : sig val get : string end) : sig
  exception Error

  val main : Lexing.lexbuf -> Parser.Make(Filename).token
end
