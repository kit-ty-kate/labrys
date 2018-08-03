(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

val t_unit_name : [`UpperName of string list]

val unit : <lib_dir : string; ..> -> Ident.Type.t
val int : <lib_dir : string; ..> -> Ident.Type.t
val float : <lib_dir : string; ..> -> Ident.Type.t
val char : <lib_dir : string; ..> -> Ident.Type.t
val string : <lib_dir : string; ..> -> Ident.Type.t

val underscore : Ident.Name.t
val underscore_loc : Location.t -> Ident.Name.t
val underscore_type_var_loc : Location.t -> Ident.Type.t
val underscore_instance_loc : Location.t -> Ident.Instance.t

val unknown_loc : Location.t

val exn : <lib_dir : string; ..> -> Ident.Type.t
val io : <lib_dir : string; ..> -> Ident.Type.t

val main : current_module:Module.t -> Ident.Name.t

val interface :
  current_module:Module.t ->
  <lib_dir : string; no_prelude : bool; ..> ->
  ParseTree.imports ->
  ParseTree.interface list ->
  (ParseTree.imports * ParseTree.interface list)

val tree :
  current_module:Module.t ->
  <lib_dir : string; no_prelude : bool; ..> ->
  ParseTree.imports ->
  ParseTree.top list ->
  (ParseTree.imports * ParseTree.top list)
