(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open Containers
open Monomorphic.None

module Matrix = PatternMatrix

type name = Ident.Name.t
type eff_name = Ident.Exn.t
type variant_name = Ident.Variant.t

type index = int

type constr = (variant_name * index)

type 'a t' =
  | Node of (int option * ('a * 'a t') list)
  | Leaf of int

type t =
  | Idx of constr t'
  | Ptr of eff_name t'

let are_any =
  let aux = function
    | Matrix.Any _ -> true
    | Matrix.Constr _ -> false
  in
  List.for_all aux

let specialize name m =
  let eq = Ident.Variant.equal in
  let size =
    let rec aux = function
      | (Matrix.Constr (_, (x, _), args) :: _, _) :: m when eq name x ->
          Int.max (List.length args) (aux m)
      | ([], _) :: m
      | (Matrix.Constr _ :: _, _) :: m
      | (Matrix.Any _ :: _, _) :: m ->
          aux m
      | [] ->
          0
    in
    aux m
  in
  let rec aux = function
    | (Matrix.Constr (_, (x, _), args) :: xs, code_index) :: m when eq name x ->
        (args @ xs, code_index) :: aux m
    | (Matrix.Constr _ :: _, _) :: m ->
        aux m
    | ((Matrix.Any _ as x) :: xs, code_index) :: m ->
        (List.replicate size x @ xs, code_index) :: aux m
    | ([], _) :: _ ->
        assert false
    | [] ->
        []
  in
  aux m

let rec var_to_idx = function
  | Matrix.VLeaf -> None
  | Matrix.VNode (idx, Matrix.VLeaf) -> Some idx
  | Matrix.VNode (_, var) -> var_to_idx var

let create ~loc envD =
  let rec aux m = match m with
    | ((Matrix.Any _ :: _ as row), code_index) :: _ when are_any row ->
        Leaf code_index
    | (Matrix.Any (var, (_, ty)) :: _, _) :: _
    | (Matrix.Constr (var, (_, ty), _) :: _, _) :: _->
        let variants = EnvMap.Constr.find ty envD in
        let (_, variants) =
          Option.get_lazy (fun () -> assert false) variants
        in
        let variants =
          let aux name (_, index) acc =
            let xs =
              match specialize name m with
              | ([], code_index) :: _ -> Leaf code_index
              | [] ->
                  (* TODO: Be more precise *)
                  Err.fail
                    ~loc
                    "Pattern non-exostive on constructor '%s'"
                    (Ident.Variant.to_string name)
              | m -> aux m
            in
            ((name, index), xs) :: acc
          in
          EnvMap.Index.fold aux variants []
        in
        Node (var_to_idx var, variants)
    | ([], _) :: _
    | [] ->
        assert false
  in
  aux

let rec get_unused_cases results = function
  | Leaf i ->
      List.remove results ~x:i
  | Node (_, l) ->
      List.fold_left (fun r (_, p) -> get_unused_cases r p) results l

let create ~loc f env ty patterns =
  let (head, tail) = match patterns with
    | [] -> assert false
    | x::xs -> (x, xs)
  in
  let (initial_pattern, initial_ty, effect) =
    let (head_p, head_t) = head in
    let (pattern, env) = Matrix.create env ty head_p in
    let (term, ty_term, effect) = f env head_t in
    ([(pattern, term)], ty_term, effect)
  in
  let (patterns, effect) =
    let f (patterns, effects) (p, t) =
      let (pattern, env) = Matrix.create env ty p in
      let (loc_t, _) = t in
      let (t, has, effect) = f env t in
      (* TODO: This should take the larger type, not only the fist one *)
      if not (Types.is_subset_of has initial_ty) then
        Types.TyErr.fail ~loc_t ~has ~expected:initial_ty;
      ((pattern, t) :: patterns, Effects.union effect effects)
    in
    List.fold_left f (initial_pattern, effect) tail
  in
  let patterns = List.rev patterns in
  let (patterns, results) = Matrix.split patterns in
  let patterns = create ~loc env.Env.constructors patterns in
  let unused_cases =
    get_unused_cases (List.mapi (fun i _ -> i) results) patterns
  in
  if not (List.is_empty unused_cases) then
    (* TODO: Be more precise *)
    Err.fail
      ~loc
      "The pattern matching contains the following unused cases (%s)"
      (Utils.string_of_list (fun x -> string_of_int (succ x)) unused_cases);
  (Idx patterns, results, initial_ty, effect)
