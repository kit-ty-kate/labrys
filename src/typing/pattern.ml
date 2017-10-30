(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type ty = TypedEnv.nty
type loc = Location.t
type name = Ident.Name.t
type constr_name = Ident.Constr.t
type index = int
type branch = int

type pattern' =
  | Wildcard
  | Constr of (loc * constr_name * pattern list)
  | Or of (pattern * pattern)
  | As of (pattern * name)
and pattern = (ty * pattern')

type tree =
  | Switch of ((loc * constr_name * ty * tree) list * tree option)
  | Swap of (index * tree)
  | Alias of (name * tree)
  | Jump of branch

type matrix = (pattern list * branch) list

let p_as name (ps, a) =
  (List.map (fun p -> (fst p, As (p, name))) ps, a)

let specialize ~eq ~ty_size =
  let rec aux (p, a) = match p with
    | [] -> assert false (* NOTE: This is forbidden by the syntax *)
    | (_, Constr (_, c, qs))::ps when eq c -> [(qs @ ps, a)]
    | (_, Constr _)::_ -> []
    | (_, Wildcard) as p::ps -> [(List.replicate ty_size p @ ps, a)]
    | (_, Or (q1, q2))::ps -> aux (q1::ps, a) @ aux (q2::ps, a)
    | (_, As (p, name))::ps -> List.map (p_as name) (aux (p::ps, a))
  in
  fun m -> List.flatten (List.map aux m)

let decompose =
  let rec aux (p, a) = match p with
    | [] -> assert false (* NOTE: This is forbidden by the syntax *)
    | (_, Constr _)::_ -> []
    | (_, Wildcard)::ps -> [(ps, a)]
    | (_, Or (q1, q2))::ps -> aux (q1::ps, a) @ aux (q2::ps, a)
    | (_, As (p, name))::ps -> List.map (p_as name) (aux (p::ps, a))
  in
  fun m -> List.flatten (List.map aux m)

let jump a =
  let exception JumpFailed of index in
  let rec aux i = function
    | [] -> Jump a
    | (_, Constr _)::_ -> raise (JumpFailed i)
    | (_, Wildcard)::xs -> Swap (i, aux (succ i) xs)
    | (_, Or _)::_ -> assert false (* NOTE: Case removed in destruct_ors *)
    | (_, As (p, name))::xs -> Alias (name, aux i (p::xs))
  in
  fun row -> try Ok (aux 1 row) with JumpFailed i -> Error i

let rec extract_constr = function
  | (_, Wildcard) -> []
  | (ty, Constr (loc, c, ps)) -> [(loc, c, ty, List.length ps)]
  | (_, Or (p1, p2)) -> extract_constr p1 @ extract_constr p2
  | (_, As (p, _)) -> extract_constr p

let rec get_head_constrs = function
  | [] -> []
  | ([], _)::_ -> assert false (* NOTE: Cannot happen *)
  | ((p::_), _)::m -> extract_constr p @ get_head_constrs m

let destruct_ors =
  let rec aux acc a = function
    | [] -> [(acc, a)]
    | (_, (Wildcard | Constr _)) as p::ps -> aux (acc @ [p]) a ps
    | (_, Or (p1, p2))::ps -> aux acc a (p1::ps) @ aux acc a (p2::ps)
    | (_, As (p, name))::ps -> List.map (p_as name) (aux acc a (p::ps))
  in
  fun (row, a) m -> match aux [] a row with
    | [] -> assert false (* NOTE: Cannot happen *)
    | (row, a)::xs -> (row, a, xs @ m)

let rec compile = function
  | [] -> assert false (* NOTE: This is forbidden by the syntax *)
  | row::m ->
      let (row, a, m) = destruct_ors row m in
      begin match jump a row with
      | Ok t -> t
      | Error 1 -> switch m
      | Error i -> Swap (i, compile (Utils.swap_list (pred i) m))
      end
and switch m =
  let heads = get_head_constrs m in
  let default = match decompose m with
    | [] -> None
    | m -> Some (compile m)
  in
  let aux (loc, c, ty, ty_size) =
    let eq = Ident.Constr.equal c in
    let tree = compile (specialize ~eq ~ty_size m) in
    (loc, c, ty, tree)
  in
  Switch (List.map aux heads, default)
