(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

module Imports : sig
  type 'a t

  val find : string list -> 'a t -> 'a
end

type t = private
  { values : Module.t Imports.t
  ; variants : Module.t Imports.t
  ; types : Module.t Imports.t
  ; tyclasses : Module.t Imports.t
  ; instances : Module.t Imports.t
  }

val empty : t

val add_value : export:bool -> ParseTree.new_lower_name -> Module.t -> t -> t
val add_variant : export:bool -> ParseTree.new_upper_name -> Module.t -> t -> t
val add_type : export:bool -> ParseTree.new_upper_name -> Module.t -> t -> t
val add_exn : export:bool -> ParseTree.new_upper_name -> Module.t -> t -> t
val add_tyclass : export:bool -> ParseTree.new_upper_name -> Module.t -> t -> t
val add_instance : export:bool -> ParseTree.new_lower_name -> Module.t -> t -> t

val open_module : string list -> t -> t
val union : t -> t -> t
