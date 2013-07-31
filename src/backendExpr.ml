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

module Name = BackendName
module Type = BackendType

open MonadOpen

type expr = string
type top_expr = string list

let sprintf = Printf.sprintf

let load ~target ~ty ~value =
  let target = Name.to_string target in
  let ty = Type.to_string ty in
  let value = Name.to_string value in
  sprintf "  %s = load %s* %s" target ty value

let call ~target ~ty ~f ~ty_param ~param =
  let target = Name.to_string target in
  let ty = Type.to_string ty in
  let f = Name.to_string f in
  let ty_param = Type.to_string ty_param in
  let param = Name.to_string param in
  sprintf "  %s = call %s (%s)* %s(%s %s)" target ty ty_param f ty_param param

let store ~ty ~value ~target =
  let target = Name.to_string target in
  let ty = Type.to_string ty in
  let value = Name.to_string value in
  sprintf "  store %s %s, %s* %s" ty value ty target

let ret ~ty ~value =
  let ty = Type.to_string ty in
  let value = Name.to_string value in
  sprintf "  ret %s %s" ty value

let define ~ty ~name ~ty_param ~param exprs =
  let ty = Type.to_string ty in
  let name = Name.to_string name in
  let ty_param = Type.to_string ty_param in
  let param = Name.to_string param in
  sprintf "define %s %s(%s %s) {" ty name ty_param param
  :: exprs
  @ ["}"]

let define_init ~name exprs =
  let name = Name.to_string name in
  sprintf "define void %s() {" name
  :: exprs
  @ ["  ret void"; "}"]

let global ~name ~ty =
  let name = Name.to_string name in
  let ty = Type.to_string ty in
  [sprintf "%s = global %s undef" name ty]

let to_string x = String.concat "\n" (List.concat x)