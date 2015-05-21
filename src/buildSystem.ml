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

open BatteriesExceptionless
open Monomorphic.None

exception Failure

type impl_infos =
  { version : string
  ; hash : string
  ; hash_bc : string
  ; imports : (string * string) list
  }

let map_msgpack_maps =
  let aux = function
    | (`FixRaw name, value) -> (String.implode name, value)
    | _ -> raise Failure
  in
  List.map aux

let parse_impl_infos file =
  let content = IO.read_all file in
  match Msgpack.Serialize.deserialize_string content with
    | `FixMap l ->
        begin match map_msgpack_maps l with
        | [ ("version", `FixRaw version)
          ; ("hash", `Raw16 hash)
          ; ("hash-bc", `Raw16 hash_bc)
          ; ("imports", `Map32 imports)
          ] ->
            let version = String.implode version in
            let hash = String.implode hash in
            let hash_bc = String.implode hash_bc in
            let imports =
              let aux = function
                | (`Raw16 import, `Raw16 hash_import) ->
                    (String.implode import, String.implode hash_import)
                | _ ->
                    raise Failure
              in
              List.map aux imports
            in
            {version; hash; hash_bc; imports}
        | _ ->
            raise Failure
        end
    | _ ->
        raise Failure

let check_imports_hash options =
  let aux (modul, hash) =
    let modul = Module.from_string options modul in
    let hash_file = Digest.file (Module.impl_infos modul) in
    if not (String.equal hash_file hash) then
      raise Failure
  in
  List.iter aux

let check_impl options modul =
  try
    let infos = Module.impl_infos modul in
    let infos = File.with_file_in infos (parse_impl_infos) in
    let hash = Digest.file (Module.impl modul) in
    let hash_bc = Digest.file (Module.cimpl modul) in
    if not
         (String.equal infos.version Config.version
          && String.equal infos.hash hash
          && String.equal infos.hash_bc hash_bc
         )
    then
      raise Failure;
    check_imports_hash options infos.imports;
  with
  | _ -> raise Failure

let write_impl_infos imports modul =
  let version = Config.version in
  let hash = Digest.file (Module.impl modul) in
  let hash_bc = Digest.file (Module.cimpl modul) in
  let imports =
    let aux modul =
      let import = Module.to_string modul in
      let hash_import = Digest.file (Module.impl_infos modul) in
      (`Raw16 (String.explode import), `Raw16 (String.explode hash_import))
    in
    List.map aux imports
  in
  let content =
    `FixMap
      [ (`FixRaw (String.explode "version"), `FixRaw (String.explode version))
      ; (`FixRaw (String.explode "hash"), `Raw16 (String.explode hash))
      ; (`FixRaw (String.explode "hash-bc"), `Raw16 (String.explode hash_bc))
      ; (`FixRaw (String.explode "imports"), `Map32 imports)
      ]
  in
  let content = Msgpack.Serialize.serialize_string content in
  let file_name = Module.impl_infos modul in
  Utils.mkdir file_name;
  File.with_file_out file_name (fun file -> IO.write_string file content)
