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

let rec string_of_list f = function
  | [] -> ""
  | x::[] -> f x
  | x::xs -> f x ^ ", " ^ string_of_list f xs

let rec remove_last = function
  | [] | [_] -> []
  | x::xs -> x :: remove_last xs

let mkdir name =
  let aux name =
    try Unix.mkdir name 0o750
    with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  in
  let l = remove_last (String.nsplit name ~by:Filename.dir_sep) in
  let l =
    let aux acc x =
      match acc with
      | [] -> [x]
      | y::_ ->
          Filename.concat y x :: acc
    in
    List.rev (List.fold_left aux [] l)
  in
  List.iter aux l

module type MapS = sig
  include BatMap.S

  val union : 'a t -> 'a t -> 'a t
end

module Map (I : Map.OrderedType) = struct
  include Map.Make(I)

  let union x y = fold add x y
end
