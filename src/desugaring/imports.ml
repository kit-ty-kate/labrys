(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

module Imports = struct
  type 'a t = (string list * 'a) list

  let empty = []
  let add k x self = (k, x) :: self
  let find k self = List.Assoc.get_exn ~eq:(List.equal String.equal) k self
  let foldr f self acc = List.fold_right (fun (k, x) acc -> f k x acc) self acc
  let union a b = a @ b
end

type t =
  { values : Module.t Imports.t
  ; variants : Module.t Imports.t
  ; types : Module.t Imports.t
  ; tyclasses : Module.t Imports.t
  ; instances : Module.t Imports.t
  }

let empty =
  { values = Imports.empty
  ; variants = Imports.empty
  ; types = Imports.empty
  ; tyclasses = Imports.empty
  ; instances = Imports.empty
  }

let add ~export name modul imports =
  let base = if export then Module.to_module modul else [] in
  Imports.add (base @ [name]) modul imports

let add_value ~export name modul imports = match name with
  | (_, `NewLowerName name) ->
      let values = add ~export name modul imports.values in
      {imports with values}
  | (_, `Underscore) ->
      imports

let add_variant ~export (_, `NewUpperName name) modul imports =
  let variants = add ~export name modul imports.variants in
  {imports with variants}

let add_type ~export (_, `NewUpperName name) modul imports =
  let types = add ~export name modul imports.types in
  {imports with types}

let add_exn ~export (_, `NewUpperName name) modul imports =
  let variants = add ~export name modul imports.variants in
  let types = add ~export name modul imports.types in
  {imports with variants; types}

let add_tyclass ~export (_, `NewUpperName name) modul imports =
  let tyclasses = add ~export name modul imports.tyclasses in
  {imports with tyclasses}

let add_instance ~export name modul imports = match name with
  | (_, `NewLowerName name) ->
      let instances = add ~export name modul imports.instances in
      {imports with instances}
  | (_, `Underscore) ->
      imports

let open_module modul imports =
  let aux name m imports =
    let (modul', name) = Utils.detach_last name in
    if List.equal String.equal modul' modul then
      Imports.add [name] m imports
    else
      imports
  in
  let values = Imports.foldr aux imports.values imports.values in
  let variants = Imports.foldr aux imports.variants imports.variants in
  let types = Imports.foldr aux imports.types imports.types in
  let tyclasses = Imports.foldr aux imports.tyclasses imports.tyclasses in
  let instances = Imports.foldr aux imports.instances imports.instances in
  {values; variants; types; tyclasses; instances}

let union a b =
  let values = Imports.union a.values b.values in
  let variants = Imports.union a.variants b.variants in
  let types = Imports.union a.types b.types in
  let tyclasses = Imports.union a.tyclasses b.tyclasses in
  let instances = Imports.union a.instances b.instances in
  {values; variants; types; tyclasses; instances}
