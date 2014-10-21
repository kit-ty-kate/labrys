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

type t
type m

module type CONFIG = sig
  val initial_heap_size : int
end

module Module (X : sig val name : Ident.Module.t end) : sig
  val make :
    imports:ModulePath.t list ->
    UntypedTree.top list ->
    m
end

module Runtime (Conf : CONFIG) : sig
  val make : main_module:ModulePath.t -> m -> t
end

val link : m -> m -> m

val optimize : opt:int -> lto:bool -> t -> t

val to_string : t -> string

val write_bitcode : o:string -> t -> bool

val emit_object_file : tmp:string -> t -> unit

val default_heap_size : int
