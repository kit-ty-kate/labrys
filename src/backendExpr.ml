type expr = string
type top_expr = string list

let sprintf = Printf.sprintf

let load ~target ~ty ~value =
  let target = BackendValue.to_string target in
  let ty = BackendType.to_string ty in
  let value = BackendValue.to_string value in
  sprintf "  %s = load %s* %s" target ty value

let call ~target ~ty ~f ~ty_param ~param =
  let target = BackendValue.to_string target in
  let ty = BackendType.to_string_call ty in
  let f = BackendValue.to_string f in
  let ty_param = BackendType.to_string ty_param in
  let param = BackendValue.to_string param in
  sprintf "  %s = call %s %s(%s %s)" target ty f ty_param param

let store ~ty_value ~value ~ty_target ~target =
  let target = BackendValue.to_string target in
  let ty_target = BackendType.to_string ty_target in
  let ty_value = BackendType.to_string ty_value in
  let value = BackendValue.to_string value in
  sprintf "  store %s %s, %s* %s" ty_value value ty_target target

let ret ~ty ~value =
  let ty = BackendType.to_string ty in
  let value = BackendValue.to_string value in
  sprintf "  ret %s %s" ty value

let define ~ty ~name ~ty_param ~param exprs =
  let ty = BackendType.to_string ty in
  let name = BackendValue.to_string name in
  let ty_param = BackendType.to_string ty_param in
  let param = BackendValue.to_string param in
  sprintf "define %s %s(%s %s) {" ty name ty_param param
  :: exprs
  @ ["}"]

let global ~name ~ty =
  let name = BackendValue.to_string name in
  let ty = BackendType.to_string ty in
  [sprintf "%s = global %s undef" name ty]

let to_string x = String.concat "\n" (List.concat x)
