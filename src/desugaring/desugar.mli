(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

val create_imports :
  current_module:Module.t ->
  <lib_dir : string; ..> ->
  ParseTree.imports ->
  Module.t list

val create :
  current_module:Module.t ->
  <lib_dir : string; ..> ->
  Module.t list ->
  Imports.t ->
  ParseTree.top list ->
  DesugaredTree.top list

val create_interface :
  current_module:Module.t ->
  <lib_dir : string; ..> ->
  Module.t list ->
  Imports.t ->
  ParseTree.interface list ->
  (Imports.t * DesugaredTree.interface list)
