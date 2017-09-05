(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type name = Ident.Name.t
type variant_name = Ident.Variant.t

type var =
  | VLeaf
  | VNode of (int * var)

type mconstr =
  | MConstr of ((variant_name * Ident.Type.t) * mconstr list)
  | MAny of (name * Ident.Type.t)

type 'a t = (mconstr * 'a) list

type code_index = int

type pattern =
  | Constr of (var * (variant_name * Ident.Type.t) * pattern list)
  | Any of (var * (name * Ident.Type.t))

type matrix = (pattern list * code_index) list

let create =
  let rec aux env ty' = function
    | DesugaredTree.Any name ->
        let env = Env.add_value name ty' env in
        (MAny (name, fst (Types.head ty')), env)
    | DesugaredTree.TyConstr (loc, name, args) ->
        let (head_ty, tail_ty) = Types.head ty' in
        let constructors = EnvMap.Constr.find head_ty env.Env.constructors in
        let (ty_args, constructors) = Option.get_lazy (fun () -> assert false) constructors in
        let (tys, _) = EnvMap.Index.find ~head_ty name constructors in
        let aux (args, tys, env) p =
          match tys with
          | [] ->
              Err.fail ~loc "Too many parameters to this type constructor"
          | ty::xs ->
              let ty =
                let rec aux ty' = function
                  | from::xs, ty::ys ->
                      aux (Types.replace ~from ~ty ty') (xs, ys)
                  | [], [] ->
                      ty'
                  | _, [] | [], _ ->
                      assert false
                in
                aux ty (ty_args, tail_ty)
              in
              let (arg, env) = aux env ty p in
              (arg :: args, xs, env)
        in
        let (args, tys, env) = List.fold_left aux ([], tys, env) args in
        if not (List.is_empty tys) then
          Err.fail ~loc "Not enough parameters to this type constructor";
        let args = List.rev args in
        (MConstr ((name, head_ty), args), env)
  in
  aux

let succ_var = function
  | VLeaf -> VLeaf
  | VNode (i, var) -> VNode (succ i, var)

let split m =
  let rec change_row var = function
    | MConstr (name, args) :: xs ->
        let (args, names1) = change_row (VNode (0, var)) args in
        let (xs, names2) = change_row (succ_var var) xs in
        (Constr (var, name, args) :: xs, names1 @ names2)
    | MAny (name, ty) :: xs ->
        let (xs, names) = change_row (succ_var var) xs in
        (Any (var, (name, ty)) :: xs, (var, name) :: names)
    | [] ->
        ([], [])
  in
  let rec aux code_index = function
    | (row, branch) :: xs ->
        let (rows, branches) = aux (succ code_index) xs in
        let (row, names) = change_row VLeaf [row] in
        ((row, code_index) :: rows, (names, branch) :: branches)
    | [] ->
        ([], [])
  in
  aux 0 m
