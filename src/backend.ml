module Exn = MonadExn

open MonadStdlib
open Exn.Ops

type value = (string * string * BackendType.t)

type t =
  | Abs of (BackendType.t * string * value * BackendType.t * t)
  | App of (BackendType.t * t * t)
  | Val of value

let rec get_type = function
  | Types.Fun (x, ret) -> BackendType.func (get_type ret) (get_type x)
  | Types.Ty (_, name) -> name

let from_typed_tree tree =
  let rec aux i l = function
    | TypedTree.Abs (ty, (v_name, v_ty), ty_expr, t) ->
        let n = string_of_int i in
        let v_name' = "@__" ^ v_name ^ "_lambda_" ^ n in
        aux (succ i) ((v_name, v_name') :: l) t >>= fun (i, t) ->
        Exn.return (succ i, Abs (get_type ty, "@__lambda_" ^ n, (v_name', v_name, get_type v_ty), get_type ty_expr, t))
    | TypedTree.App (ty, f, x) ->
        aux i l f >>= fun (i, f) ->
        aux i l x >>= fun (i, x) ->
        Exn.return (succ i, App (get_type ty, f, x))
    | TypedTree.Val (name, ty) ->
        List.find (fun x -> Unsafe.(fst x = name)) l >>= fun x ->
        Exn.return (i, Val (snd x, name, get_type ty))
  in
  aux 1 [] tree >|= snd

let print x =
  let rec get_app_instr i ty x =
    let normal_case i ~ty_f ~name_f ~ty_x ~name_x =
      let name1 = "%" ^ string_of_int i in
      let name2 = "%" ^ string_of_int (succ i) in
      let name3 = "%" ^ string_of_int (succ (succ i)) in
      let ty = BackendType.to_string_call ty in
      let ty_x = BackendType.to_string ty_x in
      let ty_f = BackendType.to_string ty_f in
      (succ (succ i),
       name3,
       [ "  " ^ name1 ^ " = load " ^ ty_x ^ "* " ^ name_x
       ; "  " ^ name2 ^ " = load " ^ ty_f ^ "* " ^ name_f
       ; "  " ^ name3 ^ " = call " ^ ty ^ " " ^ name2 ^ "(" ^ ty_x ^ " " ^ name1 ^ ")"
       ]
      )
    in
    match x with
      | (App (ty_f, f, x), App (ty_x, f', x')) ->
          let (i, name_f, f) = get_app_instr (succ i) ty (f, x) in
          let (i, name_x, x) = get_app_instr (succ i) ty (f', x') in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, f @ x @ res)
      | (App (ty_f, f, x), Abs (ty_x, name_x, _, _, _)) ->
          let (i, name_f, f) = get_app_instr (succ i) ty (f, x) in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, f @ res)
      | (App (ty_f, f, x), Val (name_x, _, ty_x)) ->
          let (i, name_f, f) = get_app_instr (succ i) ty (f, x) in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, f @ res)
      | (Abs (ty_f, name_f, _, _, _), App (ty_x, f, x)) ->
          let (i, name_x, x) = get_app_instr (succ i) ty (f, x) in
          let (i, name, res) = normal_case i ~ty_f ~name_f ~ty_x ~name_x in
          (i, name, x @ res)
      | (Val (name_f, _, ty_f), App (ty_x, f, x)) ->
          let (i, name_x, x) = get_app_instr (succ i) ty (f, x) in
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
        ["  ret " ^ ty ^ " " ^ name]
    | App (ty, f, x) ->
        let (_, name, instr) = get_app_instr 1 ty (f, x) in
        instr
        @ ["  ret " ^ BackendType.to_string ty ^ " " ^ name]
    | Val (name', name, ty) ->
        let ty = BackendType.to_string ty in
        [ "  " ^ name ^ " = load " ^ ty ^ "* " ^ name'
        ; "  ret " ^ ty ^ " " ^ name
        ]
  in
  let rec aux = function
    | Abs (ret, name, (param', param, p_ty), _, t) ->
        let ret = BackendType.to_string ret in
        let p_ty = BackendType.to_string p_ty in
        [ param' ^ " = global " ^ p_ty ^ " undef"
        ; "define " ^ ret ^ " " ^ name ^ "(" ^ p_ty ^ " %" ^ param ^ ") {"
        ; "  store " ^ p_ty ^ " %" ^ param ^ ", " ^ p_ty ^ "* " ^ param'
        ]
        @ get_instr t
        @ ["}"]
        @ aux t
    | App (_, f, x) -> aux f @ aux x
    | Val _ -> []
  in
  print_endline (String.concat "\n" (aux x))
