(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open Containers
open Monomorphic.None

let fmt = Printf.sprintf

type t =
  | Located of (Location.t * string)
  | Module of string

exception Exn of t

let fail ~loc x = Printf.ksprintf (fun x -> raise (Exn (Located (loc, x)))) x

let fail_module x = Printf.ksprintf (fun x -> raise (Exn (Module x))) x

let dump = function
  | Located (loc, x) ->
      let string_of_location {Location.loc_start; loc_end; filename} =
        fmt
          "Error in '%s' from line %d column %d to line %d column %d:\n"
          filename
          loc_start.Location.pos_lnum
          loc_start.Location.pos_cnum
          loc_end.Location.pos_lnum
          loc_end.Location.pos_cnum
      in
      string_of_location loc ^ "    " ^ x
  | Module x ->
      fmt "Error:\n    %s" x
