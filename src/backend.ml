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

open BatteriesExceptionless
open Monomorphic.None

let c = LLVM.create_context ()
let m = LLVM.create_module c "Main"

let find_in_gamma name l =
  let c = ref 0 in
  Option.map
    (fun res -> (res, !c))
    (List.find (fun x -> incr c; String.equal (fst x) name) l)

let i8_type = LLVM.i8_type c
let i32_type = LLVM.i32_type c
let i64_type = LLVM.i64_type c
let star_type = LLVM.pointer_type i8_type
let env_type = LLVM.pointer_type star_type
let lambda_type = LLVM.function_type star_type [|star_type; env_type|]
let closure_type = LLVM.struct_type c [|LLVM.pointer_type lambda_type; env_type|]
let variant_type = LLVM.struct_type c [|i32_type; env_type|]

let i64 = LLVM.const_int i64_type
let i32 = LLVM.const_int i32_type

let create_env size builder =
  let env =
    LLVM.build_malloc
      (LLVM.array_type star_type size)
      "env"
      builder
  in
  LLVM.build_gep env [|i64 0; i64 0|] "env_cast" builder

let create_closure f env builder =
  let closure = LLVM.build_malloc closure_type "closure" builder in
  let loaded = LLVM.build_load closure "closure_loaded" builder in
  let loaded = LLVM.build_insertvalue loaded f 0 "closure_insert_f" builder in
  let loaded = LLVM.build_insertvalue loaded env 1 "closure_insert_env" builder in
  LLVM.build_store loaded closure builder;
  LLVM.build_bitcast closure star_type "closure_cast" builder

let insert_arg_in_env param env gammaEnv builder =
  let env = LLVM.build_gep env [|i32 (List.length gammaEnv)|] "gep_env" builder in
  LLVM.build_store param env builder

let rec lambda env gammaParam gammaEnv gammaGlob builder = function
  | UntypedTree.Abs (name, t) ->
      let (f, builder') = LLVM.define_function c "__lambda" lambda_type m in
      let closure = create_closure f env builder in
      let builder = builder' in
      let gammaP = (name, LLVM.param f 0) in
      let env = LLVM.param f 1 in
      insert_arg_in_env (snd gammaP) env gammaEnv builder;
      let gammaE = (name, List.length gammaEnv) in
      let v = lambda env (Some gammaP) (gammaE :: gammaEnv) gammaGlob builder t in
      LLVM.build_ret v builder;
      closure
  | UntypedTree.App (f, x) ->
      let boxed_f = lambda env gammaParam gammaEnv gammaGlob builder f in
      let boxed_f = LLVM.build_bitcast boxed_f (LLVM.pointer_type closure_type) "extract_f_cast" builder in
      let boxed_f = LLVM.build_load boxed_f "exctract_f" builder in
      let f = LLVM.build_extractvalue boxed_f 0 "f" builder in
      let env = LLVM.build_extractvalue boxed_f 1 "env" builder in
      let x = lambda env gammaParam gammaEnv gammaGlob builder x in
      LLVM.build_call f [|x; env|] "tmp" builder
  | UntypedTree.Val name ->
      let find gamma =
        Option.map (fun (res, c) -> (snd res, c)) (find_in_gamma name gamma)
      in
      let execEnv (i, _) =
        let result = LLVM.build_gep env [|i32 i|] "from_env" builder in
        LLVM.build_load result "from_env_loaded" builder
      in
      let execGlob (value, _) =
        LLVM.build_load value "glob_extract" builder
      in
      begin match gammaParam with
      | Some (name', param) when String.equal name name' ->
          param
      | Some _ | None ->
          begin match find gammaEnv with
          | Some x -> execEnv x
          | None ->
              execGlob (Option.default_delayed (fun () -> assert false) (find gammaGlob))
          end
      end
  | UntypedTree.Variant i ->
      let variant = LLVM.build_malloc variant_type "variant" builder in
      let variant_loaded = LLVM.build_load variant "variant_loaded" builder in
      let variant_loaded = LLVM.build_insertvalue variant_loaded (i32 i) 0 "variant_with_idx" builder in
      let variant_loaded = LLVM.build_insertvalue variant_loaded env 1 "variant_with_vals" builder in
      LLVM.build_store variant_loaded variant builder;
      LLVM.build_bitcast variant star_type "cast_variant" builder

let rec init gammaGlob builder = function
  | `Val (name, g, t, size) :: xs ->
      let env = create_env size builder in
      let value = lambda env None [] gammaGlob builder t in
      LLVM.build_store value g builder;
      init ((name, g) :: gammaGlob) builder xs
  | `Datatype f :: xs ->
      let gs = f builder in
      init (gs @ gammaGlob) builder xs
  | [] -> ()

let make =
  let rec top init_list gamma = function
    | UntypedTree.Value (name, t, size) :: xs ->
        let g = LLVM.define_global name (LLVM.undef star_type) m in
        top (`Val (name, g, t, size) :: init_list) gamma xs
    | UntypedTree.Binding (name, binding) :: xs ->
        let v = LLVM.bind c ~name binding in
        top init_list ((name, v) :: gamma) xs
    | [] ->
        let ty = LLVM.function_type (LLVM.void_type c) [||] in
        let (_, builder) = LLVM.define_function c "__init" ty m in
        init gamma builder (List.rev init_list);
        LLVM.build_ret_void builder;
        m
  in
  top [] []
