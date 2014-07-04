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

let fmt = Printf.sprintf

module Make (I : sig val name : Ident.Module.t end) = struct
  type gamma =
    | Value of LLVM.llvalue
    | Env of int
    | Glob of int
    | ValueGlob of (int * LLVM.llvalue)

  let c = LLVM.create_context ()
  let m = LLVM.create_module c (Ident.Module.to_module_name I.name)

  module Type = struct
    let i8 = LLVM.i8_type c
    let i32 = LLVM.i32_type c
    let star = LLVM.pointer_type i8
    let exn = LLVM.struct_type c [|LLVM.pointer_type star; star|]
    let exn_ptr = LLVM.pointer_type exn
    let lambda = function
      | true -> LLVM.function_type star [|star; star; exn_ptr|]
      | false -> LLVM.function_type star [|star; star|]
    let lambda_ptr with_exn = LLVM.pointer_type (lambda with_exn)
    let closure with_exn = LLVM.struct_type c [|lambda_ptr with_exn; star|]
    let closure_ptr with_exn = LLVM.pointer_type (closure with_exn)
    let variant = LLVM.struct_type c [|i32; star|]
    let variant_ptr = LLVM.pointer_type variant
    let array = LLVM.array_type star
    let array_ptr size = LLVM.pointer_type (array size)
    let unit_function = LLVM.function_type (LLVM.void_type c) [||]
  end

  let i32 = LLVM.const_int Type.i32
  let null = LLVM.const_null Type.star
  let string = LLVM.const_string c

  module Globals : sig
    type t

    val create : size:int -> t
    val load : t -> LLVM.llbuilder -> t
    val store : t -> LLVM.llvalue -> int -> LLVM.llbuilder -> t
    val get : t -> LLVM.llvalue
  end = struct
    type t =
      { globals : LLVM.llvalue
      ; loaded : LLVM.llvalue
      }

    let create ~size =
      let initial_value =
        let value = Array.make size null in
        LLVM.const_array Type.star value
      in
      let globals = LLVM.define_global "globals" initial_value m in
      LLVM.set_linkage LLVM.Linkage.Private globals;
      {globals; loaded = initial_value}

    let load {globals; _} builder =
      {globals; loaded = LLVM.build_load globals "" builder}

    let store {globals; loaded} x i builder =
      let loaded = LLVM.build_insertvalue loaded x i "" builder in
      LLVM.build_store loaded globals builder;
      {globals; loaded}

    let get {loaded; _} = loaded
  end

  let malloc_and_init ty values builder =
    let aux acc i x = LLVM.build_insertvalue acc x i "" builder in
    let values = List.fold_lefti aux (LLVM.undef ty) values in
    let allocated = LLVM.build_malloc ty "" builder in
    LLVM.build_store values allocated builder;
    allocated

  let malloc_and_init_array size values builder =
    match size with
    | 0 -> null
    | size -> malloc_and_init (Type.array size) values builder

  let debug_trap = LLVM.declare_function "llvm.debugtrap" Type.unit_function m

  let create_default_branch func =
    let block = LLVM.append_block c "" func in
    let builder = LLVM.builder_at_end c block in
    ignore (LLVM.build_call debug_trap [||] "" builder);
    LLVM.build_unreachable builder;
    block

  let fold_env ~env gamma builder =
    let aux name value (i, values, gamma) =
      match value with
      | Value value ->
          let values = value :: values in
          let gamma = GammaMap.Value.add name (Env i) gamma in
          (succ i, values, gamma)
      | Env j ->
          let env = Lazy.force env in
          let value = LLVM.build_extractvalue env j "" builder in
          let values = value :: values in
          let gamma = GammaMap.Value.add name (Env i) gamma in
          (succ i, values, gamma)
      | ValueGlob (j, _)
      | Glob j ->
          let gamma = GammaMap.Value.add name (Glob j) gamma in
          (i, values, gamma)
    in
    let (a, b, c) = GammaMap.Value.fold aux gamma (0, [], GammaMap.Value.empty) in
    (a, List.rev b, c)

  let create_closure ~isrec ~used_vars ~with_exn ~f ~env gamma builder =
    let aux acc i x = LLVM.build_insertvalue acc x i "" builder in
    let allocated = LLVM.build_malloc (Type.closure with_exn) "" builder in
    let gamma = GammaMap.Value.filter (fun x _ -> Set.mem x used_vars) gamma in
    let gamma = match isrec with
      | Some rec_name -> GammaMap.Value.add rec_name (Value allocated) gamma
      | None -> gamma
    in
    let (env_size, values, gamma) = fold_env ~env gamma builder in
    let env = malloc_and_init_array env_size values builder in
    let closure = List.fold_lefti aux (LLVM.undef (Type.closure with_exn)) [f; env] in
    LLVM.build_store closure allocated builder;
    (allocated, gamma)

  let get_exn name =
    let name = Ident.Name.prepend I.name name in
    let name = Ident.Name.to_string name in
    LLVM.declare_global Type.star name m

  let rec llvalue_of_pattern_var vars value builder var =
    match Map.find var vars with
    | Some value ->
        (value, vars)
    | None ->
        let (value, vars) =
          match var with
          | Pattern.VLeaf ->
              (value, vars)
          | Pattern.VNode (i, var) ->
              let (value, vars) = llvalue_of_pattern_var vars value builder var in
              let value = LLVM.build_bitcast value Type.variant_ptr "" builder in
              let value = LLVM.build_load value "" builder in
              let value = LLVM.build_extractvalue value 1 "" builder in
              let value = LLVM.build_bitcast value (Type.array_ptr (succ i)) "" builder in
              let value = LLVM.build_load value "" builder in
              (LLVM.build_extractvalue value i "" builder, vars)
        in
        (value, Map.add var value vars)

  let rec create_branch func ~env ~default vars gamma value results tree =
    let block = LLVM.append_block c "" func in
    let builder = LLVM.builder_at_end c block in
    create_tree func ~env ~default vars gamma builder value results tree;
    block

  and create_result func ~env ~exn ~globals ~res ~next_block ~exn_block gamma builder (vars, result) =
    let block = LLVM.append_block c "" func in
    let builder' = LLVM.builder_at_end c block in
    let (gamma, pattern_vars) =
      let aux (gamma, pattern_vars) (var, name) =
        let variable = LLVM.build_alloca Type.star "" builder in
        let value = LLVM.build_load variable "" builder' in
        (GammaMap.Value.add name (Value value) gamma, (var, variable) :: pattern_vars)
      in
      List.fold_left aux (gamma, []) vars
    in
    let (v, builder') = lambda func ~env ~exn ~globals ~exn_block gamma builder' result in
    LLVM.build_store v res builder';
    LLVM.build_br next_block builder';
    (block, pattern_vars)

  and create_tree func ~env ~default vars gamma builder value results = function
    | UntypedTree.Leaf i ->
        let (block, pattern_vars) = List.nth results i in
        let aux vars (var, variable) =
          let (var, vars) = llvalue_of_pattern_var vars value builder var in
          LLVM.build_store var variable builder;
          vars
        in
        ignore (List.fold_left aux vars pattern_vars);
        LLVM.build_br block builder
    | UntypedTree.Node (var, cases) ->
        let (term, vars) = llvalue_of_pattern_var vars value builder var in
        let term = LLVM.build_bitcast term Type.variant_ptr "" builder in
        let term = LLVM.build_load term "" builder in
        let term = LLVM.build_extractvalue term 0 "" builder in
        let switch = LLVM.build_switch term default (List.length cases) builder in
        List.iter
          (fun (constr, tree) ->
             let branch = create_branch func ~env ~default vars gamma value results tree in
             LLVM.add_case switch (i32 constr) branch
          )
          cases

  and create_exn_result func ~env ~exn ~globals ~res ~next_block ~exn_block ~exn_args gamma builder (args, result) =
    let block = LLVM.append_block c "" func in
    let builder' = LLVM.builder_at_end c block in
    let gamma =
      let aux gamma i name =
        let value = LLVM.build_extractvalue exn_args i "" builder in
        GammaMap.Value.add name (Value value) gamma
      in
      List.fold_lefti aux gamma args
    in
    let (v, builder') = lambda func ~env ~exn ~globals ~exn_block gamma builder' result in
    LLVM.build_store v res builder';
    LLVM.build_br next_block builder';
    block

  and create_exn_branches func ~env ~exn ~globals ~res ~next_block ~exn_block ~new_exn ~with_exn gamma builder branches =
    let new_exn = LLVM.build_load new_exn "" builder in
    let new_exn_loaded = LLVM.build_load new_exn "" builder in
    let exn_tag = LLVM.build_extractvalue new_exn_loaded 0 "" builder in
    let exn_args = LLVM.build_extractvalue new_exn_loaded 1 "" builder in
    let aux builder ((name, args), t) =
      let block = create_exn_result func ~env ~globals ~res ~next_block ~exn ~exn_block ~exn_args gamma builder (args, t) in
      let exn = get_exn name in
      let next_block = LLVM.append_block c "" func in
      let cond = LLVM.build_icmp LLVM.Icmp.Eq exn exn_tag "" builder in
      LLVM.build_cond_br cond block next_block builder;
      LLVM.builder_at_end c next_block
    in
    let builder = List.fold_left aux builder branches in
    if with_exn then begin
      let exn = Lazy.force exn in
      let exn_block = Lazy.force exn_block in
      LLVM.build_store new_exn_loaded exn builder;
      LLVM.build_br exn_block builder;
    end else begin
      ignore (LLVM.build_call debug_trap [||] "" builder);
      LLVM.build_unreachable builder;
    end

  and lambda func ?isrec ~env ~exn ~exn_block ~globals gamma builder = function
    | UntypedTree.Abs (name, with_exn, used_vars, t) ->
        let (f, builder') = LLVM.define_function c "__lambda" (Type.lambda with_exn) m in
        LLVM.set_linkage LLVM.Linkage.Private f;
        let (closure, gamma) =
          create_closure ~isrec ~used_vars ~with_exn ~f ~env gamma builder
        in
        let param = LLVM.param f 0 in
        let env = LLVM.param f 1 in
        let exn = lazy (LLVM.param f 2) in
        let env_size = GammaMap.Value.cardinal gamma in
        let env = LLVM.build_bitcast env (Type.array_ptr env_size) "" builder' in
        let env = lazy (LLVM.build_load env "" builder') in
        let globals = Globals.load globals builder' in
        let gamma = GammaMap.Value.add name (Value param) gamma in
        let exn_block =
          lazy begin
            let block = LLVM.append_block c "exn" f in
            let builder = LLVM.builder_at_end c block in
            LLVM.build_ret null builder;
            block
          end
        in
        let (v, builder') = lambda f ~env ~exn ~exn_block ~globals gamma builder' t in
        LLVM.build_ret v builder';
        (closure, builder)
    | UntypedTree.App (f, with_exn, x) ->
        let (boxed_f, builder) = lambda func ~env ~exn ~exn_block ~globals gamma builder f in
        let boxed_f = LLVM.build_bitcast boxed_f (Type.closure_ptr with_exn) "extract_f_cast" builder in
        let boxed_f = LLVM.build_load boxed_f "exctract_f" builder in
        let f = LLVM.build_extractvalue boxed_f 0 "f" builder in
        let env_f = LLVM.build_extractvalue boxed_f 1 "env" builder in
        let (x, builder) = lambda func ~env ~exn ~exn_block ~globals gamma builder x in
        begin match with_exn with
        | true ->
            let exn = Lazy.force exn in
            let res = LLVM.build_call f [|x; env_f; exn|] "tmp" builder in
            let next_block = LLVM.append_block c "" func in
            let exn_block = Lazy.force exn_block in
            let exn_block =
              let block = LLVM.append_block c "exn" func in
              let builder = LLVM.builder_at_end c block in
              LLVM.build_br exn_block builder;
              block
            in
            let exn = LLVM.build_load exn "" builder in
            let exn_is_null = LLVM.build_is_null exn "" builder in
            LLVM.build_cond_br exn_is_null next_block exn_block builder;
            (res, LLVM.builder_at_end c next_block)
        | false ->
            (LLVM.build_call f [|x; env_f|] "tmp" builder, builder)
        end
    | UntypedTree.PatternMatching (t, results, tree) ->
        let (t, builder) = lambda func ~env ~globals ~exn ~exn_block gamma builder t in
        let res = LLVM.build_alloca Type.star "" builder in
        let next_block = LLVM.append_block c "" func in
        let results = List.map (create_result func ~env ~globals ~res ~next_block ~exn ~exn_block gamma builder) results in
        let builder' = LLVM.builder_at_end c next_block in
        let default = create_default_branch func in
        create_tree func ~env ~default Map.empty gamma builder t results tree;
        (LLVM.build_load res "" builder', builder')
    | UntypedTree.Val name ->
        begin match GammaMap.Value.find name gamma with
        | Some (ValueGlob (_, value))
        | Some (Value value) ->
            (value, builder)
        | Some (Env i) ->
            let env = Lazy.force env in
            (LLVM.build_extractvalue env i "" builder, builder)
        | Some (Glob i) ->
            let globals = Globals.get globals in
            (LLVM.build_extractvalue globals i "" builder, builder)
        | None ->
            let name = Ident.Name.to_string name in
            let extern = LLVM.declare_global Type.star name m in
            (LLVM.build_load extern "" builder, builder)
        end
    | UntypedTree.Variant i ->
        let (env_size, values, _) = fold_env ~env gamma builder in
        let env = malloc_and_init_array env_size values builder in
        (malloc_and_init Type.variant [i32 i; env] builder, builder)
    | UntypedTree.Let (name, t, xs) ->
        let (t, builder) = lambda func ~env ~globals ~exn ~exn_block gamma builder t in
        let gamma = GammaMap.Value.add name (Value t) gamma in
        lambda func ~env ~globals ~exn ~exn_block gamma builder xs
    | UntypedTree.LetRec (name, t, xs) ->
        let (t, builder) = lambda func ~isrec:name ~env ~globals ~exn ~exn_block gamma builder t in
        let gamma = GammaMap.Value.add name (Value t) gamma in
        lambda func ~env ~globals ~exn ~exn_block gamma builder xs
    | UntypedTree.Fail (name, args) ->
        let aux (acc, builder) x =
          let (x, builder) = lambda func ~env ~globals ~exn ~exn_block gamma builder x in
          (x :: acc, builder)
        in
        let (args, builder) = List.fold_left aux ([], builder) args in
        let args = List.rev args in
        let args = malloc_and_init_array (List.length args) args builder in
        let tag = get_exn name in
        let v = malloc_and_init Type.exn [tag; args] builder in
        let exn = Lazy.force exn in
        let exn_block = Lazy.force exn_block in
        LLVM.build_store v exn builder;
        LLVM.build_br exn_block builder;
        (null, LLVM.builder_at_end c (LLVM.append_block c "" func))
    | UntypedTree.Try (t, with_exn, branches) ->
        let res = LLVM.build_alloca Type.star "" builder in
        let new_exn = LLVM.build_alloca Type.exn_ptr "" builder in
        let new_exn_block = LLVM.append_block c "" func in
        let (t, builder) = lambda func ~env ~globals ~exn:(lazy new_exn) ~exn_block:(lazy new_exn_block) gamma builder t in
        let next_block = LLVM.append_block c "" func in
        let non_exn_block =
          let block = LLVM.append_block c "" func in
          let builder = LLVM.builder_at_end c block in
          LLVM.build_store t res builder;
          LLVM.build_br next_block builder;
          block
        in
        let exn_is_null = LLVM.build_is_null new_exn "" builder in
        LLVM.build_cond_br exn_is_null non_exn_block new_exn_block builder;
        let builder = LLVM.builder_at_end c new_exn_block in
        create_exn_branches func ~env ~exn ~globals ~res ~next_block ~exn_block ~new_exn ~with_exn gamma builder branches;
        let builder = LLVM.builder_at_end c next_block in
        (LLVM.build_load res "" builder, builder)

  let lambda func ~globals gamma builder t =
    let env = lazy (assert false) in
    let exn = lazy (assert false) in
    let exn_block = lazy (assert false) in
    let globals = Globals.load globals builder in
    lambda func ~env ~exn ~globals ~exn_block gamma builder t

  let rec init func ~globals gamma global_values builder = function
    | `Val (name, i, t) :: xs ->
        let (value, builder) = lambda func ~globals gamma builder t in
        let gamma = GammaMap.Value.add name (ValueGlob (i, value)) gamma in
        let global_values = GammaMap.Value.add name value global_values in
        let globals = Globals.store globals value i builder in
        init func ~globals gamma global_values builder xs
    | `Rec (name, i, t) :: xs ->
        let gamma = GammaMap.Value.add name (Glob i) gamma in
        let (value, builder) = lambda func ~globals gamma builder t in
        let global_values = GammaMap.Value.add name value global_values in
        let globals = Globals.store globals value i builder in
        init func ~globals gamma global_values builder xs
    | `Bind (name, value, i) :: xs ->
        let gamma = GammaMap.Value.add name (Glob i) gamma in
        let value = LLVM.build_load value "" builder in
        let global_values = GammaMap.Value.add name value global_values in
        let globals = Globals.store globals value i builder in
        init func ~globals gamma global_values builder xs
    | [] ->
        let aux name value =
          let name = Ident.Name.prepend I.name name in
          let name = Ident.Name.to_string name in
          let global = LLVM.define_global name null m in
          LLVM.build_store value global builder;
        in
        GammaMap.Value.iter aux global_values;
        builder

  let () =
    let malloc_type = (LLVM.function_type Type.star [|Type.i32|]) in
    let (malloc, builder) = LLVM.define_function c "malloc" malloc_type m in
    LLVM.set_linkage LLVM.Linkage.Private malloc;
    let gc_malloc = LLVM.declare_function "GC_malloc" malloc_type m in
    LLVM.build_ret (LLVM.build_call gc_malloc (LLVM.params malloc) "" builder) builder

  let init_gc builder =
    let gc_init = LLVM.declare_function "GC_init" Type.unit_function m in
    ignore (LLVM.build_call gc_init [||] "" builder)

  let init_name name = fmt "__%s_init" (Ident.Module.to_module_name name)

  let init_imports imports builder =
    let aux import =
      let import = ModulePath.to_module import in
      let f = LLVM.declare_global Type.unit_function (init_name import) m in
      ignore (LLVM.build_call f [||] "" builder)
    in
    List.iter aux imports

  let make ~with_main ~imports =
    let rec top init_list i gamma = function
      | UntypedTree.Value (name, t) :: xs ->
          top (`Val (name, i, t) :: init_list) (succ i) gamma xs
      | UntypedTree.RecValue (name, t) :: xs ->
          top (`Rec (name, i, t) :: init_list) (succ i) gamma xs
      | UntypedTree.Binding (name, binding) :: xs ->
          let v = LLVM.bind c ~name binding m in
          top (`Bind (name, v, i) :: init_list) (succ i) gamma xs
      | UntypedTree.Exception name :: xs ->
          let name = Ident.Name.prepend I.name name in
          let name = Ident.Name.to_string name in
          let v = LLVM.define_global name (string name) m in
          LLVM.set_global_constant true v;
          top init_list i gamma xs
      | [] ->
          let (f, builder) =
            LLVM.define_function c (init_name I.name) Type.unit_function m
          in
          init_imports imports builder;
          let globals = Globals.create ~size:i in
          let builder = init f ~globals gamma GammaMap.Value.empty builder (List.rev init_list) in
          LLVM.build_ret_void builder;
          if with_main then begin
            let (_, builder) = LLVM.define_function c "main" Type.unit_function m in
            init_gc builder;
            ignore (LLVM.build_call f [||] "" builder);
            LLVM.build_ret_void builder;
          end;
          m
    in
    top [] 0 GammaMap.Value.empty
end

let make ~with_main ~name ~imports x =
  let module Module = Make(struct let name = name end) in
  Module.make ~with_main ~imports x

let link dst src =
  Llvm_linker.link_modules dst src Llvm_linker.Mode.DestroySource;
  dst

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
