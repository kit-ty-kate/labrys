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
          ] ->
            let version = String.implode version in
            let hash = String.implode hash in
            let hash_bc = String.implode hash_bc in
            {version; hash; hash_bc}
        | _ ->
            raise Failure
        end
    | _ ->
        raise Failure

let check_impl ~build_dir modul =
  try
    let infos = ModulePath.impl_infos ~build_dir modul in
    let infos = File.with_file_in infos (parse_impl_infos) in
    let hash = Digest.file (ModulePath.impl modul) in
    let hash_bc = Digest.file (ModulePath.cimpl ~build_dir modul) in
    if not
         (String.equal infos.version Config.version
          && String.equal infos.hash hash
          && String.equal infos.hash_bc hash_bc
         )
    then
      raise Failure
  with
  | _ -> raise Failure

let write_impl_infos ~build_dir modul =
  let version = Config.version in
  let hash = Digest.file (ModulePath.impl modul) in
  let hash_bc = Digest.file (ModulePath.cimpl ~build_dir modul) in
  let content =
    `FixMap
      [ (`FixRaw (String.explode "version"), `FixRaw (String.explode version))
      ; (`FixRaw (String.explode "hash"), `Raw16 (String.explode hash))
      ; (`FixRaw (String.explode "hash-bc"), `Raw16 (String.explode hash_bc))
      ]
  in
  let content = Msgpack.Serialize.serialize_string content in
  File.with_file_out
    (ModulePath.impl_infos ~build_dir modul)
    (fun file -> IO.write_string file content)
