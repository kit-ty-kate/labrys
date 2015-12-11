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

module Imports : sig
  type 'a t

  val find : string list -> 'a t -> 'a
end

type t = private
  { values : Module.t Imports.t
  ; variants : Module.t Imports.t
  ; types : Module.t Imports.t
  ; exns : Module.t Imports.t
  ; tyclasses : Module.t Imports.t
  ; instances : Module.t Imports.t
  }

val empty : t

val add_value : export:bool -> ParseTree.new_lower_name -> Module.t -> t -> t
val add_variant : export:bool -> ParseTree.new_upper_name -> Module.t -> t -> t
val add_type : export:bool -> ParseTree.new_upper_name -> Module.t -> t -> t
val add_exn : export:bool -> ParseTree.new_upper_name -> Module.t -> t -> t
val add_tyclass : export:bool -> ParseTree.new_upper_name -> Module.t -> t -> t
val add_instance : export:bool -> ParseTree.new_lower_name -> Module.t -> t -> t

val open_module : string list -> t -> t
val union : t -> t -> t
