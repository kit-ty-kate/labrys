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

open MonadOpen

type expr
type top_expr

val load :
  target:BackendName.t ->
  ty:BackendType.t ->
  value:BackendName.t ->
  expr
val call :
  target:BackendName.t ->
  ty:BackendType.t ->
  f:BackendName.t ->
  ty_param:BackendType.t ->
  param:BackendName.t ->
  expr
val store :
  ty:BackendType.t ->
  value:BackendName.t ->
  target:BackendName.t ->
  expr
val ret :
  ty:BackendType.t ->
  value:BackendName.t ->
  expr

val define :
  ty:BackendType.t ->
  name:BackendName.t ->
  ty_param:BackendType.t ->
  param:BackendName.t ->
  expr list ->
  top_expr
val define_init :
  name:BackendName.t ->
  expr list ->
  top_expr
val global :
  name:BackendName.t ->
  ty:BackendType.t ->
  top_expr

val to_string : top_expr list -> string
