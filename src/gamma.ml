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

module Name = struct
  type t = string list

  let compare = List.compare String.compare

  let equal = List.eq String.equal

  let of_list x = x
  let to_string = String.concat "."
end

module Type = Name

module Value = struct
  include Map.Make(Name)
  include Exceptionless
end

module Types = struct
  include Value

  let add ~loc k x map =
    if mem k map then
      Error.fail
        ~loc
        "A module cannot contain several times the type '%s'"
        (Name.to_string k);
    add k x map
end

module Index = Value

module Constr = struct
  include Value

  let append k x map =
    match find k map with
    | None -> add k [x] map
    | Some xs -> add k (x :: xs) map
end

type ('values, 'types, 'indexes, 'constr) t =
  { values : 'values Value.t
  ; types : 'types Types.t
  ; indexes : 'indexes Index.t
  ; constructors : 'constr Constr.t
  }

let empty =
  { values = Value.empty
  ; types = Types.empty
  ; indexes = Index.empty
  ; constructors = Constr.empty
  }
