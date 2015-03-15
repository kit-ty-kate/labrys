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

module Value : sig
  include BatMap.S with type key = Ident.Name.t
  include module type of Exceptionless

  val union : (Ident.Module.t * 'a t) -> 'a t -> 'a t
  val diff : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> string list
end

module Types : sig
  include BatMap.S with type key = Ident.Type.t
  include module type of Exceptionless

  val add : loc:Location.t -> key -> 'a -> 'a t -> 'a t

  val union : (Ident.Module.t * 'a t) -> 'a t -> 'a t
  val diff : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> string list
end

module Index : module type of Value

module Constr : sig
  include BatMap.S with type key = Ident.Type.t
  include module type of Exceptionless

  val add : key -> Index.key -> 'a -> 'a Index.t t -> 'a Index.t t

  val union : (Ident.Module.t * 'a t) -> 'a t -> 'a t
  val diff : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> string list
end

module Exn : sig
  include BatMap.S with type key = Ident.Exn.t
  include module type of Exceptionless

  val add : loc:Location.t -> key -> 'a -> 'a t -> 'a t

  val union : (Ident.Module.t * 'a t) -> 'a t -> 'a t
  val diff : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> string list
end

module Eff : sig
  include BatSet.S with type elt = Ident.Eff.t
  include module type of Exceptionless

  val add : loc:Location.t -> elt -> t -> t
end
