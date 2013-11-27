(*
Copyright (c) 2013 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

module TT = TypedTree

open Batteries
open MonadOpen

let c = LLVM.create_context ()
let m = LLVM.create_module c "Main"

let find_in_gamma name l =
  let c = ref 0 in
  List.find (fun x -> incr c; Unsafe.(fst x = name)) l >|= fun res ->
  (res, !c)

let i64 = LLVM.const_int (LLVM.i64_type c)
let i32 = LLVM.const_int (LLVM.i32_type c)
let star_type = LLVM.pointer_type (LLVM.i8_type c)
let env_type = LLVM.pointer_type star_type
let lambda_type = LLVM.function_type star_type [|star_type; env_type|]
let closure_type = LLVM.struct_type c [|LLVM.pointer_type lambda_type; env_type|]

let create_env gammaParam gammaEnv builder =
  let env =
    LLVM.build_malloc
      (LLVM.array_type star_type (succ (List.length gammaEnv)))
      "env"
      builder
  in
  let fill_old (_, f) =
    let loaded = LLVM.build_load env "env_loaded" builder in
    let old = LLVM.param f 1 in
    let fill_env dst i x =
      let x = LLVM.build_gep old [|i32 x|] "from_env" builder in
      let x = LLVM.build_load x "from_env_loaded" builder in
      LLVM.build_insertvalue dst x i "fill_env" builder
    in
    let (loaded, _) =
      List.fold_left
        (fun (dst, i) (_, x) -> (fill_env dst i x, succ i))
        (loaded, 0)
        gammaEnv
    in
    LLVM.build_store loaded env builder
  in
  Option.may fill_old gammaParam;
  LLVM.build_gep env [|i64 0; i64 0|] "env_cast" builder

let create_closure ty f env builder =
  let closure = LLVM.build_malloc closure_type "closure" builder in
  let loaded = LLVM.build_load closure "closure_loaded" builder in
  let loaded = LLVM.build_insertvalue loaded f 0 "closure_insert_f" builder in
  let loaded = LLVM.build_insertvalue loaded env 1 "closure_insert_env" builder in
  LLVM.build_store loaded closure builder;
  LLVM.build_bitcast closure star_type "closure_cast" builder

let insert_arg_in_env f gammaEnv builder =
  let env = LLVM.param f 1 in
  let env = LLVM.build_gep env [|i32 (List.length gammaEnv)|] "gep_env" builder in
  let param = LLVM.param f 0 in
  LLVM.build_store param env builder

let rec lambda gammaParam gammaEnv gammaGlob builder = function
  | TT.Abs ({TT.abs_ty; TT.param; TT.ty_expr}, t) ->
      let (f, builder') = LLVM.define_function c "__lambda" lambda_type m in
      let env = create_env gammaParam gammaEnv builder in
      let closure = create_closure abs_ty f env builder in
      let builder = builder' in
      insert_arg_in_env f gammaEnv builder;
      let gammaP = (param.TT.name, f) in
      let gammaE = (param.TT.name, List.length gammaEnv) in
      lambda (Some gammaP) (gammaE :: gammaEnv) gammaGlob builder t >>= fun v ->
      LLVM.build_ret v builder;
      Exn.return closure
  | TT.TAbs (_, t) -> lambda gammaParam gammaEnv gammaGlob builder t
  | TT.App (ty, f, x) ->
      lambda gammaParam gammaEnv gammaGlob builder f >>= fun boxed_f ->
      let boxed_f = LLVM.build_bitcast boxed_f (LLVM.pointer_type closure_type) "extract_f_cast" builder in
      let boxed_f = LLVM.build_load boxed_f "exctract_f" builder in
      let f = LLVM.build_extractvalue boxed_f 0 "f" builder in
      let env = LLVM.build_extractvalue boxed_f 1 "env" builder in
      lambda gammaParam gammaEnv gammaGlob builder x >>= fun x ->
      Exn.return (LLVM.build_call f [|x; env|] "tmp" builder)
  | TT.TApp (_, f, _) -> lambda gammaParam gammaEnv gammaGlob builder f
  | TT.Val {TT.name; TT.ty} ->
      let find gamma =
        find_in_gamma name gamma >|= fun (res, c) -> (snd res, c)
      in
      let execEnv (i, _) = match gammaParam with
        | Some (_, f) ->
            let param = LLVM.param f 1 in
            let result = LLVM.build_gep param [|i32 i|] "from_env" builder in
            LLVM.build_load result "from_env_loaded" builder
        | None -> assert false
      in
      let execGlob (value, _) =
        LLVM.build_load value "glob_extract" builder
      in
      let res = match gammaParam with
        | Some (name', x) when String.equal name name' ->
            Exn.return (LLVM.param x 0)
        | Some _ | None -> Exn.fail `NotFound
      in
      let res =
        Exn.catch res (fun `NotFound -> find gammaEnv >|= execEnv)
      in
      let res = Exn.catch res (fun `NotFound -> find gammaGlob >|= execGlob) in
      res

let rec init gammaGlob builder = function
  | (name, g, t) :: xs ->
      lambda None [] gammaGlob builder t >>= fun value ->
      LLVM.build_store value g builder;
      init ((name, g) :: gammaGlob) builder xs
  | [] -> Exn.return ()

let create_variants =
  let rec create i gamma = function
    | Types.Ty _ ->
        LLVM.const_gep
          (LLVM.const_struct
             c
             (Array.of_list
                (LLVM.const_int (LLVM.i32_type c) i :: List.map snd gamma)
             )
          )
          [||]
    | Types.Forall (_, ret) -> create i gamma ret
    | Types.Fun (param, Types.Ty _) ->
        let (f, builder) = LLVM.define_function c "__lambda" lambda_type m in
        let result =
          LLVM.build_malloc
            (LLVM.struct_type c (Array.of_list (LLVM.i32_type c :: LLVM.i8_type c :: List.map (fun _ -> LLVM.i8_type c) gamma)))
            "result"
            builder
        in
        let result_loaded = LLVM.build_load result "result_loaded" builder in
        let result_loaded = LLVM.build_insertvalue result_loaded (LLVM.const_int (LLVM.i32_type c) i) 0 "result_loaded_is_back" builder in
        let (result_loaded, _) = List.fold_left (fun (acc, index) x -> LLVM.build_insertvalue acc (LLVM.const_int (LLVM.i32_type c) i) index "result_loaded_is_back" builder, succ index) (result_loaded, 1) gamma in
        LLVM.build_store result_loaded result builder;
        let result = LLVM.build_bitcast result star_type "cast_result" builder in
        LLVM.build_ret result builder;
        f
    | Types.Fun (param, ret) ->
        let (f, builder) = LLVM.define_function c "__lambda" lambda_type m in
        LLVM.build_ret (create i gamma ret) builder;
        f
  in
  let f i = function
    | TT.Variant (name, ty) ->
        let f = create i [] ty in
        let g = LLVM.define_global name f m in
        LLVM.set_global_constant true g;
        (name, g)
  in
  List.mapi f

let make =
  let rec top init_list gamma = function
    | TT.Value ({TT.name; TT.ty}, t) :: xs ->
        let g = LLVM.define_global name (LLVM.undef star_type) m in
        top ((name, g, t) :: init_list) gamma xs
    | TT.Binding ({TT.name; _}, binding) :: xs ->
        let v = LLVM.bind c ~name binding in
        top init_list ((name, v) :: gamma) xs
    | TT.Datatype variants :: xs ->
        top init_list (create_variants variants @ gamma) xs
    | [] ->
        let ty = LLVM.function_type (LLVM.void_type c) [||] in
        let (f, builder) = LLVM.define_function c "__init" ty m in
        init gamma builder (List.rev init_list) >>= fun () ->
        LLVM.build_ret_void builder;
        Exn.return m
  in
  top [] []
