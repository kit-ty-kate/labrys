(*
Copyright (c) 2013-2015 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

let unsafe_compare = compare

open BatteriesExceptionless
open Monomorphic.None

exception Error of string

type t =
  { library : bool
  ; src_dir : string
  ; build_dir : string
  ; modul : string list (** NOTE: Absolute module name *)
  }

let to_file = String.concat "/"

let create ~current_module modul =
  let library = false in
  let src_dir = current_module.src_dir in
  let build_dir = current_module.build_dir in
  let modul = Utils.remove_last current_module.modul @ modul in
  {library; src_dir; build_dir; modul}

let library_create options modul =
  let library = true in
  let src_dir = options.Options.lib_dir in
  let build_dir = options.Options.lib_dir in
  {library; src_dir; build_dir; modul}

let matches_module_name =
  let open Re in
  let lower = rg 'a' 'z' in
  let upper = rg 'A' 'Z' in
  let regexp = whole_string (seq [upper; rep (alt [lower; upper])]) in
  let regexp = compile regexp in
  fun modul ->
    test (exec regexp modul) 0

let module_from_string modul =
  let modul = String.nsplit modul ~by:"." in
  let is_correct str =
    if not (matches_module_name str) then
      raise (Error "The name of the module given is not correct.");
  in
  List.iter is_correct modul;
  modul

let from_string options modul =
  let library = false in
  let src_dir = options.Options.src_dir in
  let build_dir = Filename.concat options.Options.build_dir src_dir in
  let modul = module_from_string modul in
  {library; src_dir; build_dir; modul}

let library_from_string options modul =
  let library = true in
  let src_dir = options.Options.lib_dir in
  let build_dir = options.Options.lib_dir in
  let modul = module_from_string modul in
  {library; src_dir; build_dir; modul}

let impl self =
  Filename.concat self.src_dir (to_file self.modul) ^ ".sfw"

let cimpl self =
  Filename.concat self.build_dir (to_file self.modul) ^ ".bc"

let impl_infos self =
  Filename.concat self.build_dir (to_file self.modul) ^ ".csfw"

let intf self =
  Filename.concat self.src_dir (to_file self.modul) ^ ".sfwi"

let to_string self =
  String.concat "." self.modul

let to_list self =
  self.modul

let is_library self =
  self.library

type tmp = t

module Map = Map.Make(struct
    type t = tmp

    let compare x y = unsafe_compare x y
  end)
