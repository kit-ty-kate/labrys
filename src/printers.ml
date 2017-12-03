(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

let fmt = Printf.sprintf
let (^^) = PPrint.(^^)

let rec dump_top f doc = function
  | [] -> doc
  | [x] -> doc ^^ f x
  | x::xs -> dump_top f (doc ^^ f x ^^ PPrint.hardline ^^ PPrint.hardline) xs

let string_of_doc doc =
  let buf = Buffer.create 1024 in
  PPrint.ToBuffer.pretty 0.9 80 buf doc;
  Buffer.contents buf

module OptimizedTree = struct
  open OptimizedTree

  let dump_name = LIdent.to_string

  let dump_free_vars free_vars =
    let aux acc _ name =
      fmt "%s %s" acc (dump_name name)
    in
    EnvSet.MIDValue.fold free_vars "Ø" aux

  let dump_args_ty l =
    let aux = function
      | (`Int (), name) -> fmt "Int %s" (dump_name name)
      | (`Float (), name) -> fmt "Float %s" (dump_name name)
      | (`Char (), name) -> fmt "Char %s" (dump_name name)
      | (`String (), name) -> fmt "String %s" (dump_name name)
      | (`Custom, name) -> fmt "UNKNOWN %s" (dump_name name)
    in
    String.concat ", " (List.map aux l)

  let dump_tag_ty = function
    | `Int () -> "Int"
    | `Float () -> "Float"
    | `Char () -> "Char"
    | `String () -> "String"
    | `Custom -> "UNKNOWN"
    | `Void -> "VOID"

  let dump_constr_rep = function
    | Index idx -> string_of_int idx
    | Exn name -> dump_name name

  let rec dump_t' = function
    | Abs (name, free_vars, t) ->
        PPrint.group
          (PPrint.lparen
           ^^ PPrint.string
                (fmt "λ %s [%s] ->" (dump_name name) (dump_free_vars free_vars))
           ^^ PPrint.nest 2 (PPrint.break 1 ^^ dump_t t)
           ^^ PPrint.rparen
          )
    | Rec (name, t) ->
        PPrint.group
          (PPrint.lparen
           ^^ PPrint.string
                (fmt "μ %s ->" (dump_name name))
           ^^ PPrint.nest 2 (PPrint.break 1 ^^ dump_t' t)
           ^^ PPrint.rparen
          )
    | App (f, x) ->
        PPrint.group
          (PPrint.lparen
           ^^ PPrint.string (dump_name f)
           ^^ PPrint.blank 1
           ^^ PPrint.nest 2 (PPrint.break 1 ^^ PPrint.string (dump_name x))
           ^^ PPrint.rparen
          )
    | Val name ->
        PPrint.string (dump_name name)
    | Datatype (index, params) ->
        PPrint.group
          (PPrint.lbracket
           ^^ PPrint.space
           ^^ PPrint.string (Option.map_or ~default:"" dump_constr_rep index)
           ^^ PPrint.space
           ^^ PPrint.bar
           ^^ PPrint.space
           ^^ dump_args params
           ^^ PPrint.rbracket
          )
    | CallForeign (name, ret, args) ->
        PPrint.string (fmt "%s(%s) returns %s" name (dump_args_ty args) (dump_tag_ty ret))
    | PatternMatching _ ->
        assert false (* TODO *)
    | Fail name ->
        PPrint.group
          (PPrint.lparen
           ^^ PPrint.string "fail"
           ^^ PPrint.blank 1
           ^^ PPrint.string (dump_name name)
           ^^ PPrint.rparen
          )
    | Try (t, (name, t')) ->
        PPrint.group
          (PPrint.string "try"
           ^^ PPrint.break 1
           ^^ dump_t t
           ^^ PPrint.break 1
           ^^ PPrint.string "with"
          )
        ^^ PPrint.string (fmt "| %s -> " (dump_name name))
        ^^ dump_t t'
        ^^ PPrint.break 1
        ^^ PPrint.string "end"
    | RecordGet (name, n) ->
        PPrint.string (fmt "%s.%d" (dump_name name) n)
    | Const (`Int n) ->
        PPrint.string (fmt "%d" n)
    | Const (`Float n) ->
        PPrint.string (fmt "%f" n)
    | Const (`Char c) ->
        PPrint.string (fmt "'%lc'" (Uchar.to_int c))
    | Const (`String s) ->
        PPrint.string (fmt "\"%s\"" s)
    | Unreachable ->
        PPrint.string "UNREACHABLE"

  and dump_t (lets, t) =
    let aux (name, x) =
      PPrint.string (dump_name name) ^^
      PPrint.space ^^ PPrint.equals ^^ PPrint.space ^^
      dump_t' x
    in
    let lets = List.map aux lets in
    PPrint.group
      (PPrint.lbracket ^^
       List.fold_left (fun acc x -> acc ^^ x ^^ PPrint.semi ^^ PPrint.break 1) PPrint.empty lets ^^
       PPrint.rbracket ^^
       PPrint.space ^^
       dump_t' t
      )

  and dump_args args =
    let aux doc name = doc ^^ PPrint.break 1 ^^ PPrint.string (dump_name name) in
    List.fold_left aux PPrint.empty args

  let dump_linkage = function
    | Private -> "private"
    | Public -> "public"

  let dump = function
    | Value (name, t, linkage) ->
        PPrint.group
          (PPrint.string (fmt "let %s : %s =" (dump_name name) (dump_linkage linkage))
           ^^ PPrint.nest 2 (PPrint.break 1 ^^ dump_t t)
          )
    | Function (name, (name', t), linkage) ->
        PPrint.group
          (PPrint.string (fmt "function %s %s : %s =" (dump_name name) (dump_name name') (dump_linkage linkage))
           ^^ PPrint.nest 2 (PPrint.break 1 ^^ dump_t t)
          )
    | Exception name ->
        PPrint.string (fmt "exception %s" (dump_name name))

  let dump top =
    let doc = dump_top dump PPrint.empty top in
    string_of_doc doc
end
