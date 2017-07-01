(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

val arrow :
  loc:DesugaredTree.loc ->
  < lib_dir : string; .. > ->
  DesugaredTree.ty ->
  DesugaredTree.effects option ->
  DesugaredTree.ty ->
  DesugaredTree.ty'

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
  (Imports.t * InterfaceTree.t list)
