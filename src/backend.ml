module Exn = MonadExn

open MonadStdlib
open Exn.Ops

let sprintf = Printf.sprintf

type var =
  | Global of string
  | Local of string

type value = (var * string * BackendType.t)

type t =
  | Abs of (BackendType.t * var * value * BackendType.t * t)
  | App of (BackendType.t * t * t)
  | Val of value

let string_of_var = function
  | Global x -> x
  | Local x -> x

let rec get_type = function
  | Types.Fun (x, ret) -> BackendType.func (get_type ret) (get_type x)
  | Types.Ty (_, name) -> name

let from_typed_tree tree =
  let rec aux i l = function
    | TypedTree.Abs (ty, (v_name, v_ty), ty_expr, t) ->
        let n = i in
        let v_name' = Global (sprintf "@__%s_lambda_%d" v_name n) in
        aux (succ i) ((v_name, v_name') :: l) t >>= fun (i, t) ->
        Exn.return (i, Abs (get_type ty, Global (sprintf "@__lambda_%d" n), (v_name', v_name, get_type v_ty), get_type ty_expr, t))
    | TypedTree.App (ty, f, x) ->
        aux i l f >>= fun (i, f) ->
        aux (succ i) l x >>= fun (i, x) ->
        Exn.return (i, App (get_type ty, f, x))
    | TypedTree.Val (name, ty) ->
        List.find (fun x -> Unsafe.(fst x = name)) l >>= fun x ->
        Exn.return (i, Val (snd x, name, get_type ty))
  in
  aux 1 [] tree >|= snd

let print x =
  let rec get_app_instr i ty x =
    let normal_case i ~ty_f ~name_f ~ty_x ~name_x =
      let get_target i = "%" ^ string_of_int i in
      let loading i ty = function
        | Global name ->
            let target = get_target i in
            (succ i, target, [sprintf "  %s = load %s * %s" target ty name])
        | Local name -> (i, name, [])
      in
      let ty = BackendType.to_string_call ty in
      let ty_x = BackendType.to_string ty_x in
      let ty_f = BackendType.to_string ty_f in
      let (i, name1, step1) = loading i ty_x name_x in
      let (i, name2, step2) = loading i ty_f name_f in
      let name3 = get_target i in
      (succ i,
       Local name3,
       step1 @ step2 @ [sprintf "  %s = call %s %s(%s %s)" name3 ty name2 ty_x name1]
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
    | Abs (_, name, (_, _, _), ty, t) ->
        let ty = BackendType.to_string ty in
        let name = string_of_var name in
        [sprintf "  ret %s %s" ty name]
    | App (ty, f, x) ->
        let (_, name, instr) = get_app_instr 1 ty (f, x) in
        let name = string_of_var name in
        instr
        @ [sprintf "  ret %s %s" (BackendType.to_string ty) name]
    | Val (name', name, ty) ->
        let ty = BackendType.to_string ty in
        let name' = string_of_var name' in
        [ sprintf "  %s = load %s* %s" name ty name'
        ; sprintf "  ret %s %s" ty name'
        ]
  in
  let rec aux = function
    | Abs (ret, name, (param', param, p_ty), _, t) ->
        let ret = BackendType.to_string ret in
        let p_ty = BackendType.to_string p_ty in
        let name = string_of_var name in
        let param' = string_of_var param' in
        [ sprintf "%s = global %s undef" param' p_ty
        ; sprintf "define %s %s(%s %%%s) {" ret name p_ty param
        ; sprintf "  store %s %%%s, %s* %s" p_ty param p_ty param'
        ]
        @ get_instr t
        @ ["}"]
        @ aux t
    | App (_, f, x) -> aux f @ aux x
    | Val _ -> []
  in
  print_endline (String.concat "\n" (aux x))
