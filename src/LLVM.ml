type lltype =
  | Void
  | Int of string
  | Fun of (lltype * lltype list)
  | Pointer of lltype
  | Struct of lltype list
  | Array of (lltype * int)
type llvalue = Value of lltype * string
type llmodule = Module of string list Lazy.t list ref
type llbuilder = Builder of string list ref

exception TypeMissmatch of string * lltype

let fail name ty = raise (TypeMissmatch (name, ty))

let p = Printf.sprintf

let global_name =
  let c = ref 0 in
  (fun x -> incr c; "@" ^ x ^ string_of_int !c)

let local_name =
  let c = ref 0 in
  (fun x -> incr c; "%" ^ x ^ string_of_int !c)

let concat = String.concat ", "

let append_m (Module l) l' = l := !l @ l'

let append_b (Builder l) l' = l := !l @ l'

let rec string_of_ty = function
  | Void -> "void"
  | Int x -> x
  | Fun (ret, args) -> p "%s (%s)" (string_of_ty ret) (string_of_ty_list args)
  | Pointer x -> p "%s*" (string_of_ty x)
  | Struct l -> p "{ %s }" (string_of_ty_list l)
  | Array (t, i) -> p "[%d x %s]" i (string_of_ty t)

and string_of_ty_list l = concat (List.map string_of_ty l)

let ty_from_value (Value (ty, _)) = ty

let string_of_value (Value (ty, value)) = p "%s %s" (string_of_ty ty) value

let string_of_value_list l = concat (List.map string_of_value l)

let create_module _ = Module (ref [])

let function_type ret args = Fun (ret, args)

let void_type = Void

let i64_type = Int "i64"

let i32_type = Int "i32"

let i8_type = Int "i8"

let struct_type l = Struct l

let pointer_type x = Pointer x

let array_type t i = Array (t, i)

let define_global name (Value (ty, value)) m =
  let name = global_name name in
  append_m m [lazy ([p "%s = global %s %s" name (string_of_ty ty) value])];
  Value (pointer_type ty, name)

let define_function name ty m =
  let name = global_name name in
  let builder = Builder (ref []) in
  let (ret, args) = match ty with
    | Fun (ret, args) -> (string_of_ty ret, string_of_ty_list args)
    | ty -> fail "define_function" ty
  in
  let func = lazy (
    let Builder expr = builder in
    p "define %s %s(%s) {" ret name args :: !expr @ ["}"]
  ) in
  append_m m [func];
  (Value (pointer_type ty, name), builder)

let build_ret value b =
  append_b b [p "  ret %s" (string_of_value value)]

let build_ret_void b = append_b b ["  ret void"]

let build_load (Value (ty, value)) name b =
  let name = local_name name in
  append_b b [p "  %s = load %s %s" name (string_of_ty ty) value];
  let ty = match ty with
    | Pointer x -> x
    | ty -> fail "load" ty
  in
  Value (ty, name)

let build_store src dst b =
  let cast = string_of_value in
  append_b b [p "  store %s, %s" (cast src) (cast dst)]

let build_extractvalue (Value (ty, value)) i name b =
  let name = local_name name in
  append_b b [p "  %s = extractvalue %s %s, %d" name (string_of_ty ty) value i];
  let ty = match ty with
    | Struct l -> List.nth l i
    | ty -> fail "extractvalue" ty
  in
  Value (ty, name)

let build_insertvalue dst src n b =
  let cast = string_of_value in
  append_b b [p "  insertvalue %s, %s, %d" (cast dst) (cast src) n]

let build_call f args name b =
  let name = local_name name in
  let ty = match ty_from_value f with
    | Fun (ret, _)
    | Pointer (Fun (ret, _)) -> ret
    | ty -> fail "call" ty
  in
  append_b b [p "  %s = call %s(%s)" name (string_of_value f) (string_of_value_list args)];
  Value (ty, name)

let build_gep value idxs name b =
  let name = local_name name in
  append_b b [p "  %s = getelementptr %s" name (string_of_value_list (value :: idxs))];
  let ty = ty_from_value value in
  let aux = function (* HACK !!! *)
    | [_; _], Pointer (Array (ty, _)) -> Pointer ty
    | _, ty -> ty
  in
  Value (aux (idxs, ty), name)

let build_bitcast value ty name b =
  let name = local_name name in
  append_b b [p "  %s = bitcast %s to %s" name (string_of_value value) (string_of_ty ty)];
  Value (ty, name)

let build_malloc ty name b =
  let malloccall = local_name "malloccall" in
  append_b b [p "  %s = tail call i8* @malloc(i32 500)" malloccall];
  build_bitcast (Value (Pointer i8_type, malloccall)) (Pointer ty) name b

let const_int ty i = Value (ty, string_of_int i)

let const_struct l =
  let value = p "{ %s }" (string_of_value_list l) in
  Value (Struct (List.map ty_from_value l), value)

let undef ty = Value (ty, "undef")

let param (Value (ty, _)) i =
  let ty = match ty with
    | Pointer (Fun (_, args))
    | Fun (_, args) -> List.nth args i
    | ty -> fail "param" ty
  in
  Value (ty, "%" ^ (string_of_int i))

let to_string (Module l) =
  let declare_malloc = ["declare noalias i8* @malloc(i32)"] in
  String.concat "\n" (List.concat (declare_malloc :: List.map Lazy.force !l))

(*
TODO: sizeof malloc
*)
