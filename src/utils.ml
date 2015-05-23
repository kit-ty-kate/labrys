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

let detach_last =
  let rec aux = function
    | [] -> assert false
    | [last] -> ([], last)
    | x::xs ->
        let (xs, last) = aux xs in
        (x :: xs, last)
  in
  function
  | [] | [_] -> assert false
  | l -> aux l

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

let combine_compare l =
  let rec aux = function
    | [] ->
        0
    | x::xs ->
        let x = x () in
        if Int.equal x 0 then
          aux xs
        else
          x
  in
  aux l

module StrListSet = Set.Make(struct
    type t = string list

    let compare = List.compare String.compare
  end)
