(*
Copyright (c) 2013 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

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

type t =
  { base_path : string
  ; path : string
  ; file : string
  }

let of_file file =
  { base_path = Filename.dirname file
  ; path = ""
  ; file = Filename.basename file
  }

let impl self modul =
  let base_filename = Gamma.Type.to_file modul in
  let path = Filename.dirname base_filename in
  let path = if String.equal path "." then "" else path in
  let path = Filename.concat self.path path in
  let file = Filename.basename (base_filename ^ ".sfw") in
  {self with path; file}

let intf self modul =
  let self = impl self modul in
  let file = self.file ^ "i" in
  {self with file}

let to_module {base_path; path; file} =
  let file = Filename.concat path file in
  let file = Filename.chop_extension file in
  let file = String.nsplit file ~by:Filename.dir_sep in
  let file = List.map String.capitalize file in
  Gamma.Type.of_list file

let to_string {base_path; path; file} =
  Filename.concat base_path (Filename.concat path file)
