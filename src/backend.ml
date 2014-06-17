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

module Make (I : sig val name : string end) = struct
  type gamma =
    | Value of LLVM.llvalue
    | Env of int
    | Glob of int
    | ValueGlob of (int * LLVM.llvalue)

  let c = LLVM.create_context ()
  let m = LLVM.create_module c I.name

  module Type = struct
    let i8 = LLVM.i8_type c
    let i32 = LLVM.i32_type c
    let star = LLVM.pointer_type i8
    let lambda = LLVM.function_type star [|star; star|]
    let lambda_ptr = LLVM.pointer_type lambda
    let closure = LLVM.struct_type c [|lambda_ptr; star|]
    let closure_ptr = LLVM.pointer_type closure
    let variant = LLVM.struct_type c [|i32; star|]
    let variant_ptr = LLVM.pointer_type variant
    let array = LLVM.array_type star
    let array_ptr size = LLVM.pointer_type (array size)
    let unit_function = LLVM.function_type (LLVM.void_type c) [||]
  end

  let i32 = LLVM.const_int Type.i32
  let null = LLVM.const_null Type.star

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

  (* TODO: Use the module name as prefix for global values *)

  let malloc_and_init ty values builder =
    let aux acc i x = LLVM.build_insertvalue acc x i "" builder in
    let values = List.fold_lefti aux (LLVM.undef ty) values in
    let allocated = LLVM.build_malloc ty "" builder in
    LLVM.build_store values allocated builder;
    allocated

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
          let gamma = Gamma.Value.add name (Env i) gamma in
          (succ i, values, gamma)
      | Env j ->
          let env = Lazy.force env in
          let value = LLVM.build_extractvalue env j "" builder in
          let values = value :: values in
          let gamma = Gamma.Value.add name (Env i) gamma in
          (succ i, values, gamma)
      | ValueGlob (j, _)
      | Glob j ->
          let gamma = Gamma.Value.add name (Glob j) gamma in
          (i, values, gamma)
    in
    Gamma.Value.fold aux gamma (0, [], Gamma.Value.empty)

  let create_closure ~isrec ~used_vars ~f ~env gamma builder =
    let aux acc i x = LLVM.build_insertvalue acc x i "" builder in
    let allocated = LLVM.build_malloc Type.closure "" builder in
    let gamma = Gamma.Value.filter (fun x _ -> List.mem x used_vars) gamma in
    let gamma = match isrec with
      | Some rec_name -> Gamma.Value.add rec_name (Value allocated) gamma
      | None -> gamma
    in
    let (env_size, values, gamma) = fold_env ~env gamma builder in
    let values = List.rev values in
    let env = malloc_and_init (Type.array env_size) values builder in
    let closure = List.fold_lefti aux (LLVM.undef Type.closure) [f; env] in
    LLVM.build_store closure allocated builder;
    (allocated, gamma)

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

  and create_result func ~env ~globals ~res ~next_block gamma builder (vars, result) =
    let block = LLVM.append_block c "" func in
    let builder' = LLVM.builder_at_end c block in
    let (gamma, pattern_vars) =
      let aux (gamma, pattern_vars) (var, name) =
        let variable = LLVM.build_alloca Type.star "" builder in
        let value = LLVM.build_load variable "" builder' in
        (Gamma.Value.add name (Value value) gamma, (var, variable) :: pattern_vars)
      in
      List.fold_left aux (gamma, []) vars
    in
    let (v, builder') = lambda func ~env ~globals gamma builder' result in
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

  and lambda func ?isrec ~env ~globals gamma builder = function
    | UntypedTree.Abs (name, used_vars, t) ->
        let (f, builder') = LLVM.define_function c "__lambda" Type.lambda m in
        LLVM.set_linkage LLVM.Linkage.Private f;
        let (closure, gamma) =
          create_closure ~isrec ~used_vars ~f ~env gamma builder
        in
        let param = LLVM.param f 0 in
        let env = LLVM.param f 1 in
        let env_size = Gamma.Value.cardinal gamma in
        let env = LLVM.build_bitcast env (Type.array_ptr env_size) "" builder' in
        let env = lazy (LLVM.build_load env "" builder') in
        let globals = Globals.load globals builder' in
        let gamma = Gamma.Value.add name (Value param) gamma in
        let (v, builder') = lambda f ~env ~globals gamma builder' t in
        LLVM.build_ret v builder';
        (closure, builder)
    | UntypedTree.App (f, x) ->
        let (boxed_f, builder) = lambda func ~env ~globals gamma builder f in
        let boxed_f = LLVM.build_bitcast boxed_f Type.closure_ptr "extract_f_cast" builder in
        let boxed_f = LLVM.build_load boxed_f "exctract_f" builder in
        let f = LLVM.build_extractvalue boxed_f 0 "f" builder in
        let env_f = LLVM.build_extractvalue boxed_f 1 "env" builder in
        let (x, builder) = lambda func ~env ~globals gamma builder x in
        (LLVM.build_call f [|x; env_f|] "tmp" builder, builder)
    | UntypedTree.PatternMatching (t, results, tree) ->
        let (t, builder) = lambda func ~env ~globals gamma builder t in
        let res = LLVM.build_alloca Type.star "" builder in
        let next_block = LLVM.append_block c "" func in
        let results = List.map (create_result func ~env ~globals ~res ~next_block gamma builder) results in
        let builder' = LLVM.builder_at_end c next_block in
        let default = create_default_branch func in
        create_tree func ~env ~default Map.empty gamma builder t results tree;
        (LLVM.build_load res "" builder', builder')
    | UntypedTree.Val name ->
        let value = Gamma.Value.find name gamma in
        let value = Option.default_delayed (fun () -> assert false) value in
        begin match value with
        | ValueGlob (_, value)
        | Value value ->
            (value, builder)
        | Env i ->
            let env = Lazy.force env in
            (LLVM.build_extractvalue env i "" builder, builder)
        | Glob i ->
            let globals = Globals.get globals in
            (LLVM.build_extractvalue globals i "" builder, builder)
        end
    | UntypedTree.Variant i ->
        let (env_size, values, _) = fold_env ~env gamma builder in
        let values = List.rev values in
        let env = malloc_and_init (Type.array env_size) values builder in
        (malloc_and_init Type.variant [i32 i; env] builder, builder)
    | UntypedTree.Let (name, t, xs) ->
        let (t, builder) = lambda func ~env ~globals gamma builder t in
        let gamma = Gamma.Value.add name (Value t) gamma in
        lambda func ~env ~globals gamma builder xs
    | UntypedTree.LetRec (name, t, xs) ->
        let (t, builder) = lambda func ~isrec:name ~env ~globals gamma builder t in
        let gamma = Gamma.Value.add name (Value t) gamma in
        lambda func ~env ~globals gamma builder xs

  let lambda func ~globals gamma builder t =
    let env = lazy null in
    let globals = Globals.load globals builder in
    lambda func ~env ~globals gamma builder t

  let rec init func ~globals gamma global_values builder = function
    | `Val (name, i, t) :: xs ->
        let (value, builder) = lambda func ~globals gamma builder t in
        let gamma = Gamma.Value.add name (ValueGlob (i, value)) gamma in
        let global_values = Gamma.Value.add name value global_values in
        let globals = Globals.store globals value i builder in
        init func ~globals gamma global_values builder xs
    | `Rec (name, i, t) :: xs ->
        let gamma = Gamma.Value.add name (Glob i) gamma in
        let (value, builder) = lambda func ~globals gamma builder t in
        let global_values = Gamma.Value.add name value global_values in
        let globals = Globals.store globals value i builder in
        init func ~globals gamma global_values builder xs
    | `Bind (name, value, i) :: xs ->
        let gamma = Gamma.Value.add name (Glob i) gamma in
        let value = LLVM.build_load value "" builder in
        let global_values = Gamma.Value.add name value global_values in
        let globals = Globals.store globals value i builder in
        init func ~globals gamma global_values builder xs
    | [] ->
        (* TODO: Use global (needs a real build-system) *)
        let aux name value =
          let name = Gamma.Name.to_string name in
          let name = I.name ^ "_" ^ name in
          let global = LLVM.define_global name null m in
          LLVM.build_store value global builder;
        in
        Gamma.Value.iter aux global_values;
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

  let init_name = fmt "__%s_init"

  let init_imports imports builder =
    let aux import =
      let import = Gamma.Type.to_module_name import in
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
      | [] ->
          let (f, builder) =
            LLVM.define_function c (init_name I.name) Type.unit_function m
          in
          init_imports imports builder;
          let globals = Globals.create ~size:i in
          let builder = init f ~globals gamma Gamma.Value.empty builder (List.rev init_list) in
          LLVM.build_ret_void builder;
          if with_main then begin
            let (_, builder) = LLVM.define_function c "main" Type.unit_function m in
            init_gc builder;
            ignore (LLVM.build_call f [||] "" builder);
            LLVM.build_ret_void builder;
          end;
          m
    in
    top [] 0 Gamma.Value.empty
end

let make ~with_main ~name ~imports x =
  let name = Gamma.Type.to_module_name name in
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
