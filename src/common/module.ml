(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

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
  let src_dir = options#lib_dir in
  let build_dir = options#lib_dir in
  {library; src_dir; build_dir; modul}

let matches_module_name =
  let open Re in
  let lower = rg 'a' 'z' in
  let upper = rg 'A' 'Z' in
  let regexp = whole_string (seq [upper; rep (alt [lower; upper])]) in
  let regexp = compile regexp in
  fun modul ->
    Group.test (exec regexp modul) 0

let module_from_string modul =
  let modul = String.Split.list_cpy modul ~by:"." in
  let is_correct str =
    if not (matches_module_name str) then
      raise (Error "The name of the module given is not correct.");
  in
  List.iter is_correct modul;
  modul

let from_module_name options modul =
  let library = false in
  let src_dir = options#src_dir in
  let src_dir =
    if String.equal src_dir Filename.current_dir_name then "" else src_dir
  in
  let build_dir = Filename.concat options#build_dir src_dir in
  let modul = module_from_string modul in
  {library; src_dir; build_dir; modul}

let from_filename options modul =
  match String.chop_suffix ~suf:".sfw" modul with
  | None -> raise (Error "The expected suffix for a labrys module is .sfw")
  | Some modul -> from_module_name options modul

let library_from_module_name options modul =
  let library = true in
  let src_dir = options#lib_dir in
  let build_dir = options#lib_dir in
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

let is_library self =
  self.library

let equal x y = List.equal String.equal x.modul y.modul
let compare x y = List.compare String.compare x.modul y.modul

let to_module self = self.modul

type tmp = t

module Map = Map.Make (struct
    type t = tmp
    let compare = compare
  end)
