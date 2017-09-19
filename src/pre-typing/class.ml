(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type name = Ident.Name.t
type tyvar_name = Ident.TypeVar.t

module Instances = PrivateTypes.Instances

(* TODO: Handle contraints *)
type t = PrivateTypes.class_t =
  { params : (tyvar_name * PrivateTypes.kind) list
  ; signature : (name * PrivateTypes.t) list
  ; instances : name Instances.t
  }

let create params signature =
  { params
  ; signature
  ; instances = Instances.empty
  }

let equal = PrivateTypes.class_equal

let get_params ~loc f env tyvars args self =
  let env =
    EnvMap.TypeVar.fold Env.add_type_var tyvars env
  in
  try
    let aux (env, args) (_, k) ty =
      let loc = fst ty in
      let (ty, k') = f env ty in
      if not (PrivateTypes.kind_equal k k') then
        PrivateTypes.Err.kind ~loc ~has:k' ~expected:k;
      (env, ty :: args)
    in
    let (env, args) =
      List.fold_left2 aux (env, []) self.params args
    in
    (env, List.rev args)
  with
  | Invalid_argument _ ->
      Err.fail
        ~loc
        "Wrong number of parameter. Has %d but expected %d"
        (List.length args)
        (List.length self.params)

let get_instance_name ~loc ~tyclass tys self =
  match Instances.find tys self.instances with
  | Some x -> x
  | None ->
      Err.fail
        ~loc
        "No instance found for '%s %s'"
        (Ident.TyClass.to_string tyclass)
        (String.concat " " (List.map PrivateTypes.ty_to_string tys))

(* TODO: Improve loc *)
let add_instance ~tyclass ~current_module tys self =
  let aux (ty, k1) (_, k2) =
    if not (PrivateTypes.kind_equal k1 k2) then
      PrivateTypes.Err.kind
        ~loc:(Ident.TyClass.loc tyclass)
        ~has:k1
        ~expected:k2;
    ty
  in
  let tys = List.map2 aux tys self.params in
  let name =
    let names = List.map PrivateTypes.ty_to_string tys in
    let names = Ident.TyClass.to_string tyclass :: names in
    let name = "$instance$" ^ String.concat "$" names in
    Ident.Name.create ~loc:Builtins.unknown_loc current_module name
  in
  let instances = Instances.add tys name self.instances in
  (name, tys, {self with instances})

let get_values ~loc tys values self =
  let aux (acc, n) ((name1, t), ty1) (name2, ty2) =
    if not (Ident.Name.equal name1 name2) then
      Err.fail
        ~loc:(Ident.Name.loc name1)
        "Name missmatch. Has '%s' but expected '%s'"
        (Ident.Name.to_string name1)
        (Ident.Name.to_string name2);
    let ty2 =
      let aux ty2 (from, _) ty = PrivateTypes.ty_replace ~from ~ty ty2 in
      try
        List.fold_left2 aux ty2 self.params tys
      with
      | Invalid_argument _ -> assert false
    in
    if not (PrivateTypes.ty_equal ty1 ty2) then
      Err.fail
        ~loc:(Ident.Name.loc name1)
        "Type missmatch. Has '%s' but expected '%s'"
        (PrivateTypes.ty_to_string ty1)
        (PrivateTypes.ty_to_string ty2);
    ((name1, t) :: acc, succ n)
  in
  try
    let (values, _) = List.fold_left2 aux ([], 0) values self.signature in
    List.rev values
  with
  | Invalid_argument _ ->
      Err.fail ~loc "Signatures missmatch"
