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

type ty = (string * Llvm.lltype)

type t =
  | Fun of (t * t)
  | Ty of ty

type env = (string * t)

let context = LLVM.create_context ()

let rec to_string = function
  | Fun (Ty (x, _), ret) -> x ^ " -> " ^ to_string ret
  | Fun (x, ret) -> "(" ^ to_string x ^ ") -> " ^ to_string ret
  | Ty (x, _) -> x

let from_parse_tree gamma =
  let rec aux = function
    | ParseTree.Fun (x, y) ->
        aux x >>= fun x ->
        aux y >>= fun y ->
        Exn.return (Fun (x, y))
    | ParseTree.Ty name ->
        List.find (fun x -> Unsafe.(fst x = name)) gamma >>= fun x ->
        Exn.return (snd x)
  in
  aux

let equal = Unsafe.(=)

let gamma =
  let int = "Int" in
  [ (int, Ty (int, LLVM.i32_type context))
  ]

let env = LLVM.pointer_type (LLVM.pointer_type (LLVM.i8_type context))

let rec to_llvm ?(malloc=false) = function
  | Fun (x, ret) ->
      let ty = LLVM.function_type (to_llvm ret) [| to_llvm x; env |] in
      let st = LLVM.struct_type context [| LLVM.pointer_type ty; env |] in
      if malloc then st else LLVM.pointer_type st
  | Ty (_, ty) -> LLVM.pointer_type ty
