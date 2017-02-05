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

type name = Ident.Name.t
type t_name = Ident.Type.t
type tyvar_name = Ident.TypeVar.t
type exn_name = Ident.Exn.t
type tyclass_name = Ident.TyClass.t
type instance_name = Ident.Instance.t
type ty = DesugaredTree.ty
type loc = Location.t
type variant = DesugaredTree.variant
type tyclass_instance = DesugaredTree.tyclass_instance

type t =
  | Val of (name * ty)
  | AbstractType of (t_name * Kinds.t)
  | Datatype of (t_name * Kinds.t * (tyvar_name * Kinds.t) list * variant list)
  | TypeAlias of (t_name * ty)
  | Exception of (exn_name * ty list)
  | Class of (tyclass_name * (tyvar_name * Kinds.t) list * (name * ty) list)
  | Instance of (tyclass_instance * instance_name option)
