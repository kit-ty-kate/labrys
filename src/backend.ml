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

  let c = LLVM.create_context ()
  let m = LLVM.create_module c (Ident.Module.to_module_name I.name)

  module Type = struct
    let void = LLVM.void_type c
    let i1 = LLVM.i1_type c
    let i8 = LLVM.i8_type c
    let i32 = LLVM.i32_type c
    let i8_ptr = LLVM.pointer_type i8
    let array = LLVM.array_type i8_ptr
    let array_ptr size = LLVM.pointer_type (array size)
    let value size = LLVM.struct_type c [|i1; i32; i8_ptr; array size|]
    let value_ptr size = LLVM.pointer_type (value size)
    (** Note: jmp_buf is a five word buffer (see the LLVM doc). *)
    let jmp_buf = LLVM.array_type i8_ptr 5
    let jmp_buf_ptr = LLVM.pointer_type jmp_buf
    let lambda ~with_exn = match with_exn with
      | true ->
          LLVM.function_type i8_ptr [|i8_ptr; i8_ptr; jmp_buf_ptr|]
      | false ->
          LLVM.function_type i8_ptr [|i8_ptr; i8_ptr|]
    let lambda_ptr ~with_exn = LLVM.pointer_type (lambda ~with_exn)
    let unit_function = LLVM.function_type void [||]
    let main_function = LLVM.function_type i32 [||]
  end

  let i1 = LLVM.const_int Type.i1
  let i32 = LLVM.const_int Type.i32
  let null = LLVM.const_null Type.i8_ptr
  let undef = LLVM.undef Type.i8_ptr
  let string = LLVM.const_string c

  let fill ty values builder =
    let aux acc i x = LLVM.build_insertvalue acc x i "" builder in
    List.fold_lefti aux (LLVM.undef ty) values

  let init ptr ty values builder =
    let values = fill ty values builder in
    LLVM.build_store values ptr builder

  let malloc_and_init hd tl builder =
    let size = succ (List.length tl) in
    let ty = Type.value size in
    let allocated = LLVM.build_malloc ty "" builder in
    let value = fill (Type.array size) (hd :: tl) builder in
    init allocated ty [i1 0; i32 (pred size); null; value] builder;
    allocated

  let malloc_and_init_array size values builder =
    match size with
    | 0 ->
        null
    | size ->
        let ty = Type.array size in
        let allocated = LLVM.build_malloc ty "" builder in
        init allocated ty values builder;
        allocated

  let debug_trap = LLVM.declare_function "llvm.debugtrap" Type.unit_function m

  let longjmp =
    let ty = LLVM.function_type Type.void [|Type.i8_ptr|] in
    LLVM.declare_function "llvm.eh.sjlj.longjmp" ty m

  let setjmp =
    let ty = LLVM.function_type Type.i32 [|Type.i8_ptr|] in
    LLVM.declare_function "llvm.eh.sjlj.setjmp" ty m

  let frameaddress =
    let ty = LLVM.function_type Type.i8_ptr [|Type.i32|] in
    LLVM.declare_function "llvm.frameaddress" ty m

  let stacksave =
    let ty = LLVM.function_type Type.i8_ptr [||] in
    LLVM.declare_function "llvm.stacksave" ty m

  let exn_tag_var =
    let v = LLVM.define_global "exn_tag" null m in
    LLVM.set_thread_local true v;
    LLVM.set_linkage LLVM.Linkage.Link_once_odr v;
    v

  let exn_args_var =
    let v = LLVM.define_global "exn_args" null m in
    LLVM.set_thread_local true v;
    LLVM.set_linkage LLVM.Linkage.Link_once_odr v;
    v

  let create_default_branch func =
    let block = LLVM.append_block c "" func in
    let builder = LLVM.builder_at_end c block in
    LLVM.build_call_void debug_trap [||] builder;
    LLVM.build_unreachable builder;
    block

  let init_jmp_buf jmp_buf builder =
    let v = LLVM.undef Type.jmp_buf in
    let fp = LLVM.build_call frameaddress [|i32 0|] "" builder in
    let v = LLVM.build_insertvalue v fp 0 "" builder in
    let sp = LLVM.build_call stacksave [||] "" builder in
    let v = LLVM.build_insertvalue v sp 2 "" builder in
    LLVM.build_store v jmp_buf builder

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
    in
    let (_, b, c) = GammaMap.Value.fold aux gamma (1, [], GammaMap.Value.empty) in
    (List.rev b, c)

  let create_closure ~isrec ~with_exn ~used_vars ~env gamma builder =
    let gamma = GammaMap.Value.filter (fun x _ -> Set.mem x used_vars) gamma in
    let (values, gamma) = fold_env ~env gamma builder in
    let env_size = List.length values in
    let gamma = match isrec with
      | Some rec_name when Set.mem rec_name used_vars ->
          GammaMap.Value.add rec_name (Env 0) gamma
      | Some _ | None ->
          gamma
    in
    let env_size = succ env_size in
    let (f, builder') = LLVM.define_function c "__lambda" (Type.lambda ~with_exn) m in
    LLVM.set_linkage LLVM.Linkage.Private f;
    let f' = LLVM.build_bitcast f Type.i8_ptr "" builder in
    let closure = malloc_and_init f' values builder in
    (f, env_size, builder', closure, gamma)

  let get_exn name =
    let name = Ident.Name.prepend I.name name in
    let name = Ident.Name.to_string name in
    LLVM.declare_global Type.i8 name m

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
              let i = succ i in
              let (value, vars) = llvalue_of_pattern_var vars value builder var in
              let value = LLVM.build_bitcast value (Type.value_ptr (succ i)) "" builder in
              let value = LLVM.build_load value "" builder in
              let value = LLVM.build_extractvalue value 3 "" builder in
              (LLVM.build_extractvalue value i "" builder, vars)
        in
        (value, Map.add var value vars)

  let rec create_branch func ~env ~default vars gamma value results tree =
    let block = LLVM.append_block c "" func in
    let builder = LLVM.builder_at_end c block in
    create_tree func ~env ~default vars gamma builder value results tree;
    block

  and create_result func ~env ~jmp_buf ~res ~next_block gamma builder (vars, result) =
    let block = LLVM.append_block c "" func in
    let builder' = LLVM.builder_at_end c block in
    let (gamma, pattern_vars) =
      let aux (gamma, pattern_vars) (var, name) =
        let variable = LLVM.build_alloca Type.i8_ptr "" builder in
        let value = LLVM.build_load variable "" builder' in
        (GammaMap.Value.add name (Value value) gamma, (var, variable) :: pattern_vars)
      in
      List.fold_left aux (gamma, []) vars
    in
    let (v, builder') = lambda func ~env ~jmp_buf gamma builder' result in
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
        let term = LLVM.build_bitcast term (Type.value_ptr 1) "" builder in
        let term = LLVM.build_load term "" builder in
        let term = LLVM.build_extractvalue term 3 "" builder in
        let term = LLVM.build_extractvalue term 0 "" builder in
        let term = LLVM.build_ptrtoint term Type.i32 "" builder in
        let switch = LLVM.build_switch term default (List.length cases) builder in
        List.iter
          (fun (constr, tree) ->
             let branch = create_branch func ~env ~default vars gamma value results tree in
             LLVM.add_case switch (i32 constr) branch
          )
          cases

  and create_exn_result func ~env ~jmp_buf ~res ~next_block ~exn_args gamma (args, result) =
    let block = LLVM.append_block c "" func in
    let builder = LLVM.builder_at_end c block in
    let exn_args = LLVM.build_bitcast exn_args (Type.array_ptr (List.length args)) "" builder in
    let exn_args = lazy (LLVM.build_load exn_args "" builder) in
    let gamma =
      let aux gamma i name =
        let exn_args = Lazy.force exn_args in
        let value = LLVM.build_extractvalue exn_args i "" builder in
        GammaMap.Value.add name (Value value) gamma
      in
      List.fold_lefti aux gamma args
    in
    let (v, builder) = lambda func ~env ~jmp_buf gamma builder result in
    LLVM.build_store v res builder;
    LLVM.build_br next_block builder;
    block

  and create_exn_branches func ~env ~jmp_buf ~res ~next_block ~with_exn gamma builder branches =
    let exn_tag = LLVM.build_load exn_tag_var "" builder in
    let exn_args = LLVM.build_load exn_args_var "" builder in
    let aux builder ((name, args), t) =
      let block = create_exn_result func ~env ~res ~next_block ~jmp_buf ~exn_args gamma (args, t) in
      let exn = get_exn name in
      let next_block = LLVM.append_block c "" func in
      let cond = LLVM.build_icmp LLVM.Icmp.Eq exn exn_tag "" builder in
      LLVM.build_cond_br cond block next_block builder;
      LLVM.builder_at_end c next_block
    in
    let builder = List.fold_left aux builder branches in
    if with_exn then begin
      let jmp_buf = Lazy.force jmp_buf in
      let jmp_buf = LLVM.build_bitcast jmp_buf Type.i8_ptr "" builder in
      LLVM.build_call_void longjmp [|jmp_buf|] builder;
    end else begin
      LLVM.build_call_void debug_trap [||] builder;
    end;
    LLVM.build_unreachable builder;

  and abs ~f ~env_size ~name t gamma builder =
    let param = LLVM.param f 0 in
    let env = LLVM.param f 1 in
    let env = LLVM.build_bitcast env (Type.value_ptr env_size) "" builder in
    let env = LLVM.build_load env "" builder in
    let env = LLVM.build_extractvalue env 3 "" builder in
    let env = lazy env in
    let jmp_buf = lazy (LLVM.param f 2) in
    let gamma = GammaMap.Value.add name (Value param) gamma in
    let (v, builder) = lambda f ~env ~jmp_buf gamma builder t in
    LLVM.build_ret v builder

  and lambda func ?isrec ~env ~jmp_buf gamma builder = function
    | UntypedTree.Abs (name, with_exn, used_vars, t) ->
        let (f, env_size, builder', closure, gamma) =
          create_closure ~isrec ~with_exn ~used_vars ~env gamma builder
        in
        abs ~f ~env_size ~name t gamma builder';
        let closure = LLVM.build_bitcast closure Type.i8_ptr "" builder in
        (closure, builder)
    | UntypedTree.App (f, with_exn, x) ->
        let (closure, builder) = lambda func ~env ~jmp_buf gamma builder f in
        let (x, builder) = lambda func ~env ~jmp_buf gamma builder x in
        let f = LLVM.build_bitcast closure (Type.value_ptr 1) "" builder in
        let f = LLVM.build_load f "" builder in
        let f = LLVM.build_extractvalue f 3 "" builder in
        let f = LLVM.build_extractvalue f 0 "" builder in
        let f = LLVM.build_bitcast f (Type.lambda_ptr ~with_exn) "" builder in
        begin match with_exn with
        | true ->
            let jmp_buf = Lazy.force jmp_buf in
            (LLVM.build_call f [|x; closure; jmp_buf|] "" builder, builder)
        | false ->
            (LLVM.build_call f [|x; closure|] "" builder, builder)
        end
    | UntypedTree.PatternMatching (t, results, tree) ->
        let (t, builder) = lambda func ~env ~jmp_buf gamma builder t in
        let res = LLVM.build_alloca Type.i8_ptr "" builder in
        let next_block = LLVM.append_block c "" func in
        let results = List.map (create_result func ~env ~res ~next_block ~jmp_buf gamma builder) results in
        let builder' = LLVM.builder_at_end c next_block in
        let default = create_default_branch func in
        create_tree func ~env ~default Map.empty gamma builder t results tree;
        (LLVM.build_load res "" builder', builder')
    | UntypedTree.Val name ->
        begin match GammaMap.Value.find name gamma with
        | Some (Value value) ->
            (value, builder)
        | Some (Env i) ->
            let env = Lazy.force env in
            (LLVM.build_extractvalue env i "" builder, builder)
        | None ->
            let name = Ident.Name.prepend I.name name in
            let name = Ident.Name.to_string name in
            let extern = LLVM.declare_global Type.i8_ptr name m in
            (LLVM.build_load extern "" builder, builder)
        end
    | UntypedTree.Variant (i, params) ->
        let aux x =
          match GammaMap.Value.find x gamma with
          | Some (Value x) -> x
          | Some (Env i) ->
              let env = Lazy.force env in
              LLVM.build_extractvalue env i "" builder
          | None ->
              assert false
        in
        let values = List.map aux params in
        let i = LLVM.build_inttoptr (i32 i) Type.i8_ptr "" builder in
        let value = malloc_and_init i values builder in
        let value = LLVM.build_bitcast value Type.i8_ptr "" builder in
        (value, builder)
    | UntypedTree.Let (name, t, xs) ->
        let (t, builder) = lambda func ~env ~jmp_buf gamma builder t in
        let gamma = GammaMap.Value.add name (Value t) gamma in
        lambda func ~env ~jmp_buf gamma builder xs
    | UntypedTree.LetRec (name, t, xs) ->
        let (t, builder) = lambda func ~isrec:name ~env ~jmp_buf gamma builder t in
        let gamma = GammaMap.Value.add name (Value t) gamma in
        lambda func ~env ~jmp_buf gamma builder xs
    | UntypedTree.Fail (name, args) ->
        let aux (acc, builder) x =
          let (x, builder) = lambda func ~env ~jmp_buf gamma builder x in
          (x :: acc, builder)
        in
        let (args, builder) = List.fold_left aux ([], builder) args in
        let args = List.rev args in
        let args = malloc_and_init_array (List.length args) args builder in
        let tag = get_exn name in
        LLVM.build_store args exn_args_var builder;
        LLVM.build_store tag exn_tag_var builder;
        let jmp_buf = Lazy.force jmp_buf in
        let jmp_buf = LLVM.build_bitcast jmp_buf Type.i8_ptr "" builder in
        LLVM.build_call_void longjmp [|jmp_buf|] builder;
        LLVM.build_unreachable builder;
        (undef, LLVM.builder_at_end c (LLVM.append_block c "" func))
    | UntypedTree.Try (t, with_exn, branches) ->
        let jmp_buf' = LLVM.build_alloca Type.jmp_buf "" builder in
        init_jmp_buf jmp_buf' builder;
        let res = LLVM.build_alloca Type.i8_ptr "" builder in
        let next_block = LLVM.append_block c "" func in
        let try_block =
          let block = LLVM.append_block c "" func in
          let builder = LLVM.builder_at_end c block in
          let (t, builder) = lambda func ~env ~jmp_buf:(lazy jmp_buf') gamma builder t in
          LLVM.build_store t res builder;
          LLVM.build_br next_block builder;
          block
        in
        let catch_block =
          let block = LLVM.append_block c "" func in
          let builder = LLVM.builder_at_end c block in
          create_exn_branches func ~env ~jmp_buf ~res ~next_block ~with_exn gamma builder branches;
          block
        in
        let jmp_buf' = LLVM.build_bitcast jmp_buf' Type.i8_ptr "" builder in
        let jmp_res = LLVM.build_call setjmp [|jmp_buf'|] "" builder in
        let cond = LLVM.build_icmp LLVM.Icmp.Eq jmp_res (i32 0) "" builder in
        LLVM.build_cond_br cond try_block catch_block builder;
        let builder = LLVM.builder_at_end c next_block in
        (LLVM.build_load res "" builder, builder)

  let lambda func gamma builder t =
    let env = lazy (assert false) in
    let jmp_buf = lazy (assert false) in
    lambda func ~env ~jmp_buf gamma builder t

  let set_linkage v = function
    | UntypedTree.Private -> LLVM.set_linkage LLVM.Linkage.Private v
    | UntypedTree.Public -> ()

  let define_global ~name ~linkage value =
    let name = Ident.Name.prepend I.name name in
    let name = Ident.Name.to_string name in
    let name' = "." ^ name in
    let v = LLVM.define_global name' value m in
    LLVM.set_linkage LLVM.Linkage.Private v;
    LLVM.set_global_constant true v;
    let v = LLVM.define_global name (LLVM.const_bitcast v Type.i8_ptr) m in
    set_linkage v linkage;
    LLVM.set_global_constant true v

  type init =
    | Val of (LLVM.llvalue * UntypedTree.t)
    | Const of (unit -> unit)

  let rec init func builder = function
    | Val (global, t) :: xs ->
        let (value, builder) = lambda func GammaMap.Value.empty builder t in
        LLVM.build_store value global builder;
        init func builder xs
    | Const g :: xs ->
        g ();
        init func builder xs
    | [] ->
        builder

  let () =
    let malloc_type = (LLVM.function_type Type.i8_ptr [|Type.i32|]) in
    let (malloc, builder) = LLVM.define_function c "malloc" malloc_type m in
    LLVM.set_linkage LLVM.Linkage.Private malloc;
    let gc_malloc = LLVM.declare_function "GC_malloc" malloc_type m in
    LLVM.build_ret (LLVM.build_call gc_malloc (LLVM.params malloc) "" builder) builder

  let init_gc builder =
    let gc_init = LLVM.declare_function "GC_init" Type.unit_function m in
    LLVM.build_call_void gc_init [||] builder

  let init_name name = fmt "__%s_init" (Ident.Module.to_module_name name)

  let init_imports imports builder =
    let aux import =
      let import = ModulePath.to_module import in
      let f = LLVM.declare_global Type.unit_function (init_name import) m in
      LLVM.build_call_void f [||] builder
    in
    List.iter aux imports

  let const_value v =
    let value = LLVM.const_array Type.i8_ptr [|v|] in
    LLVM.const_struct c [|i1 0; i32 0; null; value|]

  let make ~with_main ~imports =
    let rec top init_list = function
      | UntypedTree.Value (name, t, linkage) :: xs ->
          let name = Ident.Name.prepend I.name name in
          let name = Ident.Name.to_string name in
          let global = LLVM.define_global name null m in
          set_linkage global linkage;
          top (Val (global, t) :: init_list) xs
      | UntypedTree.Binding (name, binding, linkage) :: xs ->
          let v = LLVM.bind c ~name binding m in
          let name = Ident.Name.prepend I.name name in
          let name = Ident.Name.to_string name in
          let v = LLVM.define_global name (LLVM.const_bitcast v Type.i8_ptr) m in
          set_linkage v linkage;
          LLVM.set_global_constant true v;
          top init_list xs
      | UntypedTree.Exception name :: xs ->
          let name = Ident.Name.prepend I.name name in
          let name = Ident.Name.to_string name in
          let v = LLVM.define_global name (string name) m in
          LLVM.set_global_constant true v;
          top init_list xs
      | UntypedTree.ConstVariant (name, index, linkage) :: xs ->
          let index = LLVM.const_inttoptr (i32 index) Type.i8_ptr in
          define_global ~name ~linkage (const_value index);
          top init_list xs
      | UntypedTree.Function (name, (name', with_exn, t), linkage) :: xs ->
          let name = Ident.Name.prepend I.name name in
          let (f, builder) = LLVM.define_function c "__lambda" (Type.lambda ~with_exn) m in
          LLVM.set_linkage LLVM.Linkage.Private f;
          define_global ~name ~linkage (const_value (LLVM.const_bitcast f Type.i8_ptr));
          let g () = abs ~env_size:1 ~f ~name:name' t GammaMap.Value.empty builder in
          top (Const g :: init_list) xs
      | [] ->
          let (f, builder) =
            LLVM.define_function c (init_name I.name) Type.unit_function m
          in
          init_imports imports builder;
          let builder = init f builder (List.rev init_list) in
          LLVM.build_ret_void builder;
          if with_main then begin
            let (_, builder) = LLVM.define_function c "main" Type.main_function m in
            init_gc builder;
            LLVM.build_call_void f [||] builder;
            LLVM.build_ret (i32 0) builder;
          end;
          m
    in
    top []
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
