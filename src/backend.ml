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

module Name = BackendName
module Type = BackendType
module Expr = BackendExpr

open MonadOpen

let sprintf = Printf.sprintf

type value = (Name.t * Name.t * Type.t)

type t = (Type.t * Name.t * value * Type.t, Type.t, value) Ast.t

type top =
  | Value of ((Name.t * Type.t) * t)

let rec to_type = function
  | Ast.Fun (x, ret) -> Type.func (to_type ret) (to_type x)
  | Ast.Ty (_, name) -> name

let rec get_type = function
  | Ast.Abs ((ty, _, _, _), _) -> ty
  | Ast.App (ty, _, _) -> ty
  | Ast.Val (_, _, ty) -> ty

let from_typed_tree tree =
  let rec aux i l = function
    | Ast.Abs ((ty, (v_name, v_ty), ty_expr), t) ->
        let n = i in
        let v_name' = Name.global (sprintf "__%s_lambda_%d" v_name n) in
        aux (succ i) ((v_name, v_name') :: l) t >>= fun (i, t) ->
        Exn.return
          (i,
           Ast.Abs
             ((to_type ty,
               Name.global (sprintf "__lambda_%d" n),
               (v_name', Name.local v_name, to_type v_ty),
               to_type ty_expr
              ),
              t
             )
          )
    | Ast.App (ty, f, x) ->
        aux i l f >>= fun (i, f) ->
        aux (succ i) l x >>= fun (i, x) ->
        Exn.return (i, Ast.App (to_type ty, f, x))
    | Ast.Val (name, ty) ->
        List.find (fun x -> Unsafe.(fst x = name)) l >>= fun x ->
        Exn.return (i, Ast.Val (snd x, Name.local name, to_type ty))
  in
  let rec top i l = function
    | TypedTree.Value ((name, ty), t) :: xs ->
        aux i l t >>= fun (i, x) ->
        let v = (name, Name.global name) in
        top i (v :: l) xs >>= fun (i, xs) ->
        Exn.return (i, Value ((snd v, to_type ty), x) :: xs)
    | [] -> Exn.return (i, [])
  in
  top 1 [] tree >|= snd

let print x =
  let rec get_app_instr i ty x =
    let normal_case i ~ty_f ~name_f ~ty_x ~name_x =
      let get_target i =
        Name.local (string_of_int i)
      in
      let loading i ty =
        Name.apply
          (fun value ->
            let target = get_target i in
            (succ i, target, [Expr.load ~target ~ty ~value])
          )
          (fun name -> (i, name, []))
      in
      let (i, param, step1) = loading i ty_x name_x in
      let (i, f, step2) = loading i ty_f name_f in
      let target = get_target i in
      (succ i,
       target,
       step1 @ step2 @ [Expr.call ~target ~ty ~f ~ty_param:ty_x ~param]
      )
    in
    match x with
      | (Ast.App (ty_f, f, x), Ast.App (ty_x, f', x')) ->
          let (i, name_f, f) = get_app_instr i ty_f (f, x) in
          let (i, name_x, x) = get_app_instr i ty_x (f', x') in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, f @ x @ res)
      | (Ast.App (ty_f, f, x), Ast.Abs ((ty_x, name_x, _, _), _)) ->
          let (i, name_f, f) = get_app_instr i ty_f (f, x) in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, f @ res)
      | (Ast.App (ty_f, f, x), Ast.Val (name_x, _, ty_x)) ->
          let (i, name_f, f) = get_app_instr i ty_f (f, x) in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, f @ res)
      | (Ast.Abs ((ty_f, name_f, _, _), _), Ast.App (ty_x, f, x)) ->
          let (i, name_x, x) = get_app_instr i ty_x (f, x) in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, x @ res)
      | (Ast.Val (name_f, _, ty_f), Ast.App (ty_x, f, x)) ->
          let (i, name_x, x) = get_app_instr i ty_x (f, x) in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, x @ res)
      | (Ast.Abs ((ty_f, name_f, _, _), _), Ast.Abs ((ty_x, name_x, _, _), _))
      | (Ast.Val (name_f, _, ty_f), Ast.Val (name_x, _, ty_x))
      | (Ast.Abs ((ty_f, name_f, _, _), _), Ast.Val (name_x, _, ty_x))
      | (Ast.Val (name_f, _, ty_f), Ast.Abs ((ty_x, name_x, _, _), _)) ->
          normal_case i ~ty_f ~name_f ~ty_x ~name_x
  in
  let get_instr = function
    | Ast.Abs ((_, value, (_, _, _), ty), _) ->
        [Expr.ret ~ty ~value]
    | Ast.App (ty, f, x) ->
        let (_, name, instr) = get_app_instr 1 ty (f, x) in
        instr
        @ [Expr.ret ~ty ~value:name]
    | Ast.Val (value, target, ty) ->
        [ Expr.load ~target ~ty ~value
        ; Expr.ret ~ty ~value
        ]
  in
  let rec aux = function
    | Ast.Abs ((ret, name, (param', param, p_ty), _), t) ->
        [ Expr.global ~name:param' ~ty:p_ty
        ; Expr.define ~ty:ret ~name ~ty_param:p_ty ~param
            (Expr.store ~ty:p_ty ~value:param ~target:param'
             :: get_instr t
            )
        ]
        @ aux t
    | Ast.App (_, f, x) -> aux f @ aux x
    | Ast.Val _ -> []
  in
  let rec top = function
    | Value ((name, ty), t) :: xs ->
        let x = aux t in
        let xs = top xs in
        Expr.global ~name ~ty :: x @ xs
    | [] -> []
  in
  print_endline (Expr.to_string (top x))
