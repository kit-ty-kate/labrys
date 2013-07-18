type t =
  | Abs of (Value.t * Types.t * t)
  | App of (Types.t * t * t)
  | Val of Value.t

let rec get_type = function
  | Abs (_, ty, _) -> ty
  | App (ty, _, _) -> ty
  | Val (_, ty) -> ty

let rec from_parse_tree gamma = function
  | ParseTree.Abs (v, t) ->
      let x = from_parse_tree (v :: gamma) t in
      Abs (v, Types.Fun (snd v, get_type x), x)
  | ParseTree.App (f, x) ->
      let f = from_parse_tree gamma f in
      let x = from_parse_tree gamma x in
      (match get_type f with
        | Types.Fun (ty, _) when ty = get_type x -> App (ty, f, x)
        | Types.Fun _ -> failwith "Typechecker: Argument type doesn't match"
        | Types.Ty _ -> failwith "Typechecker: Can't apply to a non-function type"
      )
  | ParseTree.Val name ->
      let x = List.find (fun (name', _) -> name = name') gamma in
      Val x
