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

val string_of_list : ('a -> string) -> 'a list -> string

val mkdir : string -> unit

val remove_last : 'a list -> 'a list
val detach_last : 'a list -> ('a list * 'a)

val combine_compare : (unit -> int) list -> int

val exec_command : string -> string list -> int

module StrListSet : CCSet.S with type elt = string list

module type EQ = sig
  type t

  val equal : t -> t -> bool
end

module type EQMAP = sig
  type key
  type 'a t

  val empty : 'a t
  val mem : key -> 'a t -> bool
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a option
  val modify_def : 'a -> key -> ('a -> 'a) -> 'a t -> 'a t
  val bindings : 'a t -> (key * 'a) list
  val singleton : key -> 'a -> 'a t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
end

module EqMap (I : EQ) : EQMAP with type key = I.t

module type EQSET = sig
  type elt
  type t

  val empty : t
  val mem : elt -> t -> bool
  val fold : (elt -> 'b -> 'b) -> t -> 'b -> 'b
  val add : elt -> t -> t
  val singleton : elt -> t
  val of_list : elt list -> t
  val remove : elt -> t -> t
  val is_empty : t -> bool
  val cardinal : t -> int
  val map : (elt -> elt) -> t -> t
  val for_all : (elt -> bool) -> t -> bool
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val union : t -> t -> t
end

module EqSet (I : EQ) : EQSET with type elt = I.t

module CCIO : module type of CCIO
