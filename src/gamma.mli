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

module Name : sig
  type t

  val equal : t -> t -> bool

  val of_list : string list -> t
  val to_string : t -> string
  val to_file : t -> string
end

module Type : module type of Name

module Value : sig
  include BatMap.S with type key = Name.t
  include module type of Exceptionless
end

module Types : sig
  include BatMap.S with type key = Type.t
  include module type of Exceptionless

  val add : loc:Location.t -> key -> 'a -> 'a t -> 'a t
end

module Index : module type of Value

module Constr : sig
  include BatMap.S with type key = Type.t
  include module type of Exceptionless

  val append : key -> 'a -> 'a list t -> 'a list t
end

type ('values, 'types, 'indexes) t =
  { values : 'values Value.t
  ; types : 'types Types.t
  ; indexes : 'indexes Index.t
  }

val empty : ('a, 'b, 'c) t

val union : (Type.t * ('a, 'b, 'c) t) -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t

val is_empty : ('a, 'b, 'c) t -> bool
