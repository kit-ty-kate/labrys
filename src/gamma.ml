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
  { values : TypesBeta.t GammaMap.Value.t
  ; types : Types.ty GammaMap.Types.t
  ; constructors : ((TypesBeta.t * int) GammaMap.Index.t) GammaMap.Constr.t
  }

let empty =
  { values = GammaMap.Value.empty
  ; types = GammaMap.Types.empty
  ; constructors = GammaMap.Constr.empty
  }

let add_value k x self = {self with values = GammaMap.Value.add k x self.values}
let add_type ~loc k x self = {self with types = GammaMap.Types.add ~loc k x self.types}
let add_constr k k2 x self = {self with constructors = GammaMap.Constr.add k k2 x self.constructors}

let union (modul, a) b =
  { values = GammaMap.Value.union (modul, a.values) b.values
  ; types = GammaMap.Types.union (modul, a.types) b.types
  ; constructors = GammaMap.Constr.union (modul, a.constructors) b.constructors
  }

(* TODO: Remove those Pervasives.(=) *)
let is_subset_of a b =
  GammaMap.Value.diff ~eq:TypesBeta.equal a.values b.values
  @ GammaMap.Types.diff ~eq:Pervasives.(=) a.types b.types
  @ GammaMap.Constr.diff ~eq:Pervasives.(=) a.constructors b.constructors
