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

type expr = string
type top_expr = string list

let sprintf = Printf.sprintf

let load ~target ~ty ~value =
  let target = BackendValue.to_string target in
  let ty = BackendType.to_string ty in
  let value = BackendValue.to_string value in
  sprintf "  %s = load %s* %s" target ty value

let call ~target ~ty ~f ~ty_param ~param =
  let target = BackendValue.to_string target in
  let ty = BackendType.to_string_call ty in
  let f = BackendValue.to_string f in
  let ty_param = BackendType.to_string ty_param in
  let param = BackendValue.to_string param in
  sprintf "  %s = call %s %s(%s %s)" target ty f ty_param param

let store ~ty_value ~value ~ty_target ~target =
  let target = BackendValue.to_string target in
  let ty_target = BackendType.to_string ty_target in
  let ty_value = BackendType.to_string ty_value in
  let value = BackendValue.to_string value in
  sprintf "  store %s %s, %s* %s" ty_value value ty_target target

let ret ~ty ~value =
  let ty = BackendType.to_string ty in
  let value = BackendValue.to_string value in
  sprintf "  ret %s %s" ty value

let define ~ty ~name ~ty_param ~param exprs =
  let ty = BackendType.to_string ty in
  let name = BackendValue.to_string name in
  let ty_param = BackendType.to_string ty_param in
  let param = BackendValue.to_string param in
  sprintf "define %s %s(%s %s) {" ty name ty_param param
  :: exprs
  @ ["}"]

let global ~name ~ty =
  let name = BackendValue.to_string name in
  let ty = BackendType.to_string ty in
  [sprintf "%s = global %s undef" name ty]

let to_string x = String.concat "\n" (List.concat x)
