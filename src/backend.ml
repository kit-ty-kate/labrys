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

type t = Llvm.llmodule

type gamma =
  | Value of LLVM.llvalue
  | Env of int
  | Glob of LLVM.llvalue

let c = LLVM.create_context ()
let m = LLVM.create_module c "Main"

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

let env_size_of_gamma gamma =
  let aux _ = function
    | Env _ -> succ
    | Value _ | Glob _ -> identity
  in
  Gamma.Value.fold aux gamma 0

let insert_arg_in_env param env env_size builder =
  let env = LLVM.build_gep env [|i32 env_size|] "gep_env" builder in
  LLVM.build_store param env builder

let rec llvalue_of_pattern_var value builder = function
  | Pattern.VLeaf -> value
  | Pattern.VNode (i, var) ->
      let value =
        LLVM.build_bitcast value (LLVM.pointer_type variant_type) "" builder
      in
      let value = LLVM.build_load value "" builder in
      let value = LLVM.build_extractvalue value 1 "" builder in
      let value = LLVM.build_gep value [|i32 i|] "" builder in
      let value = LLVM.build_load value "" builder in
      llvalue_of_pattern_var value builder var

let rec create_branch func env gamma value term results (constr, tree) =
  let gamma = match constr with
    | UntypedTree.Constr _ -> gamma
    | UntypedTree.Any name -> Gamma.Value.add name (Value term) gamma
  in
  let block = LLVM.append_block c "" func in
  let builder = LLVM.builder_at_end c block in
  ignore (create_tree func env gamma builder value results tree);
  block

and create_result func env gamma result =
  let block = LLVM.append_block c "" func in
  let builder = LLVM.builder_at_end c block in
  ignore (lambda func env gamma builder result);
  block

and create_tree func env gamma builder value results =
  function
  | UntypedTree.Leaf i ->
      let block = List.nth results i in
      LLVM.build_br block builder
  | UntypedTree.Node (var, cases) ->
      (* The more general case is always the first one
         (as it has been reversed in Pattern.create)
      *)
      let cases = List.rev cases in
      let (default, cases) = match cases with
        | x::xs -> (x, xs)
        | [] -> assert false
      in
      let term = llvalue_of_pattern_var value builder var in
      let default_branch = create_branch func env gamma value term results default in
      let switch = LLVM.build_switch term default_branch (List.length cases) builder in
      List.iter
        (fun ((constr, _) as case) ->
           let i = match constr with
             | UntypedTree.Constr i -> i
             | UntypedTree.Any _ -> assert false
           in
           let branch = create_branch func env gamma value term results case in
           LLVM.add_case switch (i32 i) branch
        )
        cases;
      switch

and lambda func env gamma builder = function
  | UntypedTree.Abs (name, t) ->
      let (f, builder') = LLVM.define_function c "__lambda" lambda_type m in
      let closure = create_closure f env builder in
      let builder = builder' in
      let param = LLVM.param f 0 in
      let env = LLVM.param f 1 in
      let env_size = env_size_of_gamma gamma in
      insert_arg_in_env param env env_size builder;
      let gamma = Gamma.Value.add name (Value param) gamma in
      let gamma = Gamma.Value.add name (Env env_size) gamma in
      let v = lambda f env gamma builder t in
      LLVM.build_ret v builder;
      closure
  | UntypedTree.App (f, x) ->
      let boxed_f = lambda func env gamma builder f in
      let boxed_f = LLVM.build_bitcast boxed_f (LLVM.pointer_type closure_type) "extract_f_cast" builder in
      let boxed_f = LLVM.build_load boxed_f "exctract_f" builder in
      let f = LLVM.build_extractvalue boxed_f 0 "f" builder in
      let env_f = LLVM.build_extractvalue boxed_f 1 "env" builder in
      let x = lambda func env gamma builder x in
      LLVM.build_call f [|x; env_f|] "tmp" builder
  | UntypedTree.PatternMatching (t, results, tree) ->
      let t = lambda func env gamma builder t in
      let results = List.map (create_result func env gamma) results in
      create_tree func env gamma builder t results tree
  | UntypedTree.Val name ->
      let value = Gamma.Value.find name gamma in
      let value = Option.default_delayed (fun () -> assert false) value in
      begin match value with
      | Value value -> value
      | Env i ->
          let value = LLVM.build_gep env [|i32 i|] "from_env" builder in
          LLVM.build_load value "from_env_loaded" builder
      | Glob value ->
          LLVM.build_load value "glob_extract" builder
      end
  | UntypedTree.Variant i ->
      let variant = LLVM.build_malloc variant_type "variant" builder in
      let variant_loaded = LLVM.build_load variant "variant_loaded" builder in
      let variant_loaded = LLVM.build_insertvalue variant_loaded (i32 i) 0 "variant_with_idx" builder in
      let variant_loaded = LLVM.build_insertvalue variant_loaded env 1 "variant_with_vals" builder in
      LLVM.build_store variant_loaded variant builder;
      LLVM.build_bitcast variant star_type "cast_variant" builder

let rec init func gamma builder = function
  | `Val (name, g, t, size) :: xs ->
      let env = create_env size builder in
      let value = lambda func env gamma builder t in
      LLVM.build_store value g builder;
      let gamma = Gamma.Value.add name (Glob g) gamma in
      init func gamma builder xs
  | `Rec (name, g, t, size) :: xs ->
      let env = create_env size builder in
      let gamma = Gamma.Value.add name (Glob g) gamma in
      let value = lambda func env gamma builder t in
      LLVM.build_store value g builder;
      init func gamma builder xs
  | [] -> ()

let make ~with_main =
  let rec top init_list gamma = function
    | UntypedTree.Value (name, t, size) :: xs ->
        let g = LLVM.define_global (Gamma.Name.to_string name) (LLVM.undef star_type) m in
        top (`Val (name, g, t, size) :: init_list) gamma xs
    | UntypedTree.RecValue (name, t, size) :: xs ->
        let g = LLVM.define_global (Gamma.Name.to_string name) (LLVM.undef star_type) m in
        top (`Rec (name, g, t, size) :: init_list) gamma xs
    | UntypedTree.Binding (name, binding) :: xs ->
        let v = LLVM.bind c ~name binding m in
        let gamma = Gamma.Value.add name (Glob v) gamma in
        top init_list gamma xs
    | [] ->
        let ty = LLVM.function_type (LLVM.void_type c) [||] in
        let (f, builder) = LLVM.define_function c "__init" ty m in
        init f gamma builder (List.rev init_list);
        LLVM.build_ret_void builder;
        if with_main then begin
          let (_, builder) = LLVM.define_function c "main" ty m in
          ignore (LLVM.build_call f [||] "" builder);
          LLVM.build_ret_void builder;
        end;
        m
  in
  top [] Gamma.Value.empty

let init = lazy (Llvm_all_backends.initialize ())

let get_triple () =
  Lazy.force init;
  Llvm_target.Target.default_triple ()

let get_target ~triple =
  let target = Llvm_target.Target.by_triple triple in
  Llvm_target.TargetMachine.create ~triple target

let optimize ~opt ~lto m =
  let triple = get_triple () in
  let target = get_target ~triple in
  let layout = Llvm_target.TargetMachine.data_layout target in
  LLVM.set_target_triple triple m;
  LLVM.set_data_layout (Llvm_target.DataLayout.as_string layout) m;
  LLVM.optimize ~lto ~opt layout m;
  m

let to_string = Llvm.string_of_llmodule

let write_bitcode ~o m = Llvm_bitwriter.write_bitcode_file m o

let emit_object_file ~tmp m =
  let triple = get_triple () in
  let target = get_target ~triple in
  Llvm_target.TargetMachine.emit_to_file
    m
    Llvm_target.CodeGenFileType.ObjectFile
    tmp
    target
