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

type t = (ParseTree.location * string)

exception Exn of t

let fail ~loc x = Printf.ksprintf (fun x -> raise (Exn (loc, x))) x

let dump ~file (loc, x) =
  let string_of_location {ParseTree.loc_start; loc_end} =
    Printf.sprintf
      "Error in '%s' from line %d column %d to line %d column %d:\n"
      file
      loc_start.ParseTree.pos_lnum
      loc_start.ParseTree.pos_cnum
      loc_end.ParseTree.pos_lnum
      loc_end.ParseTree.pos_cnum
  in
  string_of_location loc ^ "    " ^ x
