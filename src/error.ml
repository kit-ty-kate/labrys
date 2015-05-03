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

let fmt = Printf.sprintf

type t =
  | Located of (Location.t * string)
  | Module of string

exception Exn of t

let fail ~loc x = Printf.ksprintf (fun x -> raise (Exn (Located (loc, x)))) x

let fail_module x = Printf.ksprintf (fun x -> raise (Exn (Module x))) x

let dump = function
  | Located (loc, x) ->
      let string_of_location {Location.loc_start; loc_end; filename} =
        fmt
          "Error in '%s' from line %d column %d to line %d column %d:\n"
          filename
          loc_start.Location.pos_lnum
          loc_start.Location.pos_cnum
          loc_end.Location.pos_lnum
          loc_end.Location.pos_cnum
      in
      string_of_location loc ^ "    " ^ x
  | Module x ->
      fmt "Error:\n%s" x
