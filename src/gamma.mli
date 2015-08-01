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

type t = private
  { values : Types.t GammaMap.Value.t
  ; types : Types.visibility GammaMap.Types.t
  ; constructors : (Ident.Type.t list * (Types.t list * int) GammaMap.Index.t) GammaMap.Constr.t
  ; exceptions : Types.t list GammaMap.Exn.t
  ; tyclass : Class.t GammaMap.TyClass.t
  }

val empty : <lib_dir : string; ..> -> t

val add_value : Ident.Name.t -> Types.t -> t -> t
val add_type : Ident.Type.t -> Types.visibility -> t -> t
val add_constr : Ident.Type.t -> Ident.Name.t -> Ident.Type.t list -> (Types.t list * int) -> t -> t
val add_exception : Ident.Exn.t -> Types.t list -> t -> t

val union : imported:t -> t -> t
val open_module : Module.t -> t -> t

val is_subset_of : t -> t -> string list
