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

open MonadOpen

let sprintf = Printf.sprintf

type value = (BackendValue.t * BackendValue.t * BackendType.t)

type t =
  | Abs of (BackendType.t * BackendValue.t * value * BackendType.t * t)
  | App of (BackendType.t * t * t)
  | Val of value

let rec get_type = function
  | Types.Fun (x, ret) -> BackendType.func (get_type ret) (get_type x)
  | Types.Ty (_, name) -> name

let from_typed_tree tree =
  let rec aux i l = function
    | TypedTree.Abs (ty, (v_name, v_ty), ty_expr, t) ->
        let n = i in
        let v_name' = BackendValue.global (sprintf "__%s_lambda_%d" v_name n) in
        aux (succ i) ((v_name, v_name') :: l) t >>= fun (i, t) ->
        Exn.return (i, Abs (get_type ty, BackendValue.global (sprintf "__lambda_%d" n), (v_name', BackendValue.local v_name, get_type v_ty), get_type ty_expr, t))
    | TypedTree.App (ty, f, x) ->
        aux i l f >>= fun (i, f) ->
        aux (succ i) l x >>= fun (i, x) ->
        Exn.return (i, App (get_type ty, f, x))
    | TypedTree.Val (name, ty) ->
        List.find (fun x -> Unsafe.(fst x = name)) l >>= fun x ->
        Exn.return (i, Val (snd x, BackendValue.local name, get_type ty))
  in
  aux 1 [] tree >|= snd

let print x =
  let rec get_app_instr i ty x =
    let normal_case i ~ty_f ~name_f ~ty_x ~name_x =
      let get_target i =
        BackendValue.local (string_of_int i)
      in
      let loading i ty =
        BackendValue.apply
          (fun name ->
            let target = get_target i in
            (succ i, target, [BackendExpr.load ~target ~ty ~value:name])
          )
          (fun name -> (i, name, []))
      in
      let (i, name1, step1) = loading i ty_x name_x in
      let (i, name2, step2) = loading i ty_f name_f in
      let name3 = get_target i in
      (succ i,
       name3,
       step1 @ step2 @ [BackendExpr.call ~target:name3 ~ty ~f:name2 ~ty_param:ty_x ~param:name1]
      )
    in
    match x with
      | (App (ty_f, f, x), App (ty_x, f', x')) ->
          let (i, name_f, f) = get_app_instr i ty_f (f, x) in
          let (i, name_x, x) = get_app_instr i ty_x (f', x') in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, f @ x @ res)
      | (App (ty_f, f, x), Abs (ty_x, name_x, _, _, _)) ->
          let (i, name_f, f) = get_app_instr i ty_f (f, x) in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, f @ res)
      | (App (ty_f, f, x), Val (name_x, _, ty_x)) ->
          let (i, name_f, f) = get_app_instr i ty_f (f, x) in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, f @ res)
      | (Abs (ty_f, name_f, _, _, _), App (ty_x, f, x)) ->
          let (i, name_x, x) = get_app_instr i ty_x (f, x) in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, x @ res)
      | (Val (name_f, _, ty_f), App (ty_x, f, x)) ->
          let (i, name_x, x) = get_app_instr i ty_x (f, x) in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, x @ res)
      | (Abs (ty_f, name_f, _, _, _), Abs (ty_x, name_x, _, _, _))
      | (Val (name_f, _, ty_f), Val (name_x, _, ty_x))
      | (Abs (ty_f, name_f, _, _, _), Val (name_x, _, ty_x))
      | (Val (name_f, _, ty_f), Abs (ty_x, name_x, _, _, _)) ->
          normal_case i ~ty_f ~name_f ~ty_x ~name_x
  in
  let get_instr = function
    | Abs (_, name, (_, _, _), ty, _) ->
        [BackendExpr.ret ~ty ~value:name]
    | App (ty, f, x) ->
        let (_, name, instr) = get_app_instr 1 ty (f, x) in
        instr
        @ [BackendExpr.ret ~ty ~value:name]
    | Val (name', name, ty) ->
        [ BackendExpr.load ~target:name ~ty ~value:name'
        ; BackendExpr.ret ~ty ~value:name'
        ]
  in
  let rec aux = function
    | Abs (ret, name, (param', param, p_ty), _, t) ->
        [ BackendExpr.global ~name:param' ~ty:p_ty
        ; BackendExpr.define ~ty:ret ~name ~ty_param:p_ty ~param
            (BackendExpr.store ~ty_value:p_ty ~value:param ~ty_target:p_ty ~target:param'
             :: get_instr t
            )
        ]
        @ aux t
    | App (_, f, x) -> aux f @ aux x
    | Val _ -> []
  in
  print_endline (BackendExpr.to_string (aux x))
