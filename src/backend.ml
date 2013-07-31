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

type t =
  | Abs of ((Type.t * Name.t * value * Type.t) * t)
  | App of (Type.t * t * t)
  | Val of value

type top =
  | Value of ((Name.t * Type.t) * t)

let rec to_type = function
  | Types.Fun (x, ret) -> Type.func (to_type ret) (to_type x)
  | Types.Ty (_, name) -> name

let rec get_type = function
  | Abs ((ty, _, _, _), _) -> ty
  | App (ty, _, _) -> ty
  | Val (_, _, ty) -> ty

let from_typed_tree tree =
  let rec aux i l = function
    | TypedTree.Abs ((ty, (v_name, v_ty), ty_expr), t) ->
        let n = i in
        let v_name' = Name.global (sprintf "__%s_lambda_%d" v_name n) in
        aux (succ i) ((v_name, v_name') :: l) t >>= fun (i, t) ->
        Exn.return
          (i,
           Abs
             ((to_type ty,
               Name.func (sprintf "__lambda_%d" n),
               (v_name', Name.local v_name, to_type v_ty),
               to_type ty_expr
              ),
              t
             )
          )
    | TypedTree.App (ty, f, x) ->
        aux i l f >>= fun (i, f) ->
        aux (succ i) l x >>= fun (i, x) ->
        Exn.return (i, App (to_type ty, f, x))
    | TypedTree.Val (name, ty) ->
        List.find (fun x -> Unsafe.(fst x = name)) l >>= fun x ->
        Exn.return (i, Val (snd x, Name.local name, to_type ty))
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
  let get_target i =
    Name.local (string_of_int i)
  in
  let rec get_app_instr i ty x =
    let normal_case i ~ty_f ~name_f ~ty_x ~name_x =
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
      | (App (ty_f, f, x), App (ty_x, f', x')) ->
          let (i, name_f, f) = get_app_instr i ty_f (f, x) in
          let (i, name_x, x) = get_app_instr i ty_x (f', x') in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, f @ x @ res)
      | (App (ty_f, f, x), Abs ((_, name_x, _, ty_x), _)) ->
          let (i, name_f, f) = get_app_instr i ty_f (f, x) in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, f @ res)
      | (App (ty_f, f, x), Val (name_x, _, ty_x)) ->
          let (i, name_f, f) = get_app_instr i ty_f (f, x) in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, f @ res)
      | (Abs ((_, name_f, _, ty_f), _), App (ty_x, f, x)) ->
          let (i, name_x, x) = get_app_instr i ty_x (f, x) in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, x @ res)
      | (Val (name_f, _, ty_f), App (ty_x, f, x)) ->
          let (i, name_x, x) = get_app_instr i ty_x (f, x) in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, x @ res)
      | (Abs ((_, name_f, _, ty_f), _), Abs ((_, name_x, _, ty_x), _))
      | (Val (name_f, _, ty_f), Val (name_x, _, ty_x))
      | (Abs ((_, name_f, _, ty_f), _), Val (name_x, _, ty_x))
      | (Val (name_f, _, ty_f), Abs ((_, name_x, _, ty_x), _)) ->
          normal_case i ~ty_f ~name_f ~ty_x ~name_x
  in
  let get_instr i = function
    | Abs ((_, value, (_, _, _), ty), _) ->
        (i, [Expr.ret ~ty ~value])
    | App (ty, f, x) ->
        let (i, name, instr) = get_app_instr i ty (f, x) in
        (i, instr @ [Expr.ret ~ty ~value:name])
    | Val (value, _, ty) ->
        let target = get_target i in
        (i,
         [ Expr.load ~target ~ty ~value
         ; Expr.ret ~ty ~value:target
         ]
        )
  in
  let rec aux = function
    | Abs ((ret, name, (param', param, p_ty), _), t) ->
        [ Expr.global ~name:param' ~ty:p_ty
        ; Expr.define ~ty:ret ~name ~ty_param:p_ty ~param
            (Expr.store ~ty:p_ty ~value:param ~target:param'
             :: snd (get_instr 1 t)
            )
        ]
        @ aux t
    | App (_, f, x) -> aux f @ aux x
    | Val _ -> []
  in
  let rec get_instr_init i = function
    | ((target, ty), Abs ((_, value,_, _), _)) :: xs ->
        Expr.store ~ty ~value ~target :: get_instr_init i xs
    | ((target, ty), App (ty_app, f, x)) :: xs ->
        let (i, value, instr) = get_app_instr i ty_app (f, x) in
        instr @ [Expr.store ~ty ~value ~target] @ get_instr_init i xs
    | ((target, ty), Val (value, _, _)) :: xs ->
        let tmp = get_target i in
        [ Expr.load ~target:tmp ~ty ~value
        ; Expr.store ~ty ~value:tmp ~target
        ]
        @ get_instr_init (succ i) xs
    | [] -> []
  in
  let rec top init_list = function
    | Value (((name, ty), t) as v) :: xs ->
        let x = aux t in
        let xs = top (v :: init_list) xs in
        Expr.global ~name ~ty :: x @ xs
    | [] ->
        let name = Name.func "__init" in
        [Expr.define_init ~name (get_instr_init 1 init_list)]
  in
  print_endline (Expr.to_string (top [] x))
