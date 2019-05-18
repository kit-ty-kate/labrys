(* Copyright (c) 2013-2017 The Labrys developers. *)
(* See the LICENSE file at the top-level directory. *)

module Set = LIdent.MSet

module Llvm = struct
  include Llvm
  include LLVM
end

type t = Llvm.llmodule

let fmt = Printf.sprintf
let c = Llvm.global_context ()

module type I = sig
  val name : Module.t
  val debug : bool
end

module Type = struct
  let void = Llvm.void_type c
  let i8 = Llvm.i8_type c
  let i32 = Llvm.i32_type c
  let float = Llvm.float_type c
  let star = Llvm.pointer_type i8
  let star_ptr = Llvm.pointer_type star
  let array = Llvm.array_type star
  let exn_glob = star
  let array_ptr size = Llvm.pointer_type (array size)
  let variant_ptr = array_ptr
  let closure_ptr = array_ptr

  (** Note: jmp_buf is a five word buffer (see the Llvm doc). *)
  let jmp_buf = Llvm.array_type star 5
  let jmp_buf_ptr = Llvm.pointer_type jmp_buf
  let lambda ~rt_env_size =
    Llvm.function_type star [|star; closure_ptr rt_env_size; jmp_buf_ptr|]
  let lambda_ptr ~rt_env_size =
    Llvm.pointer_type (lambda ~rt_env_size)
  let init = Llvm.function_type void [|jmp_buf_ptr|]
  let unit_function = Llvm.function_type void [||]
  let main_function = Llvm.function_type i32 [||]
end

let i32 = Llvm.const_int Type.i32
let null = Llvm.const_null Type.star
let undef = Llvm.undef Type.star
let string = Llvm.const_string c

module Generic (I : sig val m : t end) = struct
  open I

  let init_name name = fmt "__%s_init" (Module.to_string name)

  let frameaddress =
    let ty = Llvm.function_type Type.star [|Type.i32|] in
    Llvm.declare_function "llvm.frameaddress" ty m

  let stacksave =
    let ty = Llvm.function_type Type.star [||] in
    Llvm.declare_function "llvm.stacksave" ty m

  let alloc_jmp_buf builder =
    let jmp_buf = Llvm.build_alloca Type.jmp_buf "" builder in
    let v = Llvm.undef Type.jmp_buf in
    let fp = Llvm.build_call frameaddress [|i32 0|] "" builder in
    let v = Llvm.build_insertvalue v fp 0 "" builder in
    let sp = Llvm.build_call stacksave [||] "" builder in
    let v = Llvm.build_insertvalue v sp 2 "" builder in
    Llvm.build_store v jmp_buf builder;
    jmp_buf

  let malloc_type = Llvm.function_type Type.star [|Type.i32|]
  let gc_malloc = Llvm.declare_function "GC_malloc" malloc_type m
end

module Main (I : sig val initial_heap_size : int val main_module : Module.t end) = struct
  let m = Llvm.create_module c "_main_"

  module Generic = Generic (struct let m = m end)

  let malloc = Llvm.declare_function "malloc" Generic.malloc_type m

  let gc_heap = Llvm.define_global "GC_heap" (Llvm.const_null Type.star_ptr) m
  let gc_heap_cursor = Llvm.define_global "GC_heap_cursor" (Llvm.const_int Type.i32 0) m
  let gc_heap_size = Llvm.define_global "GC_heap_size" (Llvm.const_int Type.i32 0) m

  let fill_gc_malloc builder =
    (* TODO: Implement the GC (free!!) *)
    let size = Llvm.current_param builder 0 in
    let current_heap_cursor = Llvm.build_load gc_heap_cursor "" builder in
    let current_heap_size = Llvm.build_load gc_heap_size "" builder in
    let new_heap_cursor = Llvm.build_add current_heap_cursor size "" builder in
    let enough_space = Llvm.build_icmp Llvm.Icmp.Ule new_heap_cursor current_heap_size "" builder in
    let return =
      let (block, builder) = Llvm.create_block c builder in
      let current_heap = Llvm.build_load gc_heap "" builder in
      let ptr = Llvm.build_gep current_heap [|current_heap_cursor|] "" builder in
      Llvm.build_store new_heap_cursor gc_heap_cursor builder;
      Llvm.build_ret ptr builder;
      block
    in
    let extend_space =
      let (block, builder) = Llvm.create_block c builder in
      let new_heap_size = Llvm.build_add current_heap_size size "" builder in
      let new_heap = Llvm.build_call malloc [|new_heap_size|] "" builder in
      (* TODO: Handle malloc failures *)
      Llvm.build_store new_heap gc_heap builder;
      Llvm.build_store size gc_heap_cursor builder;
      Llvm.build_store new_heap_size gc_heap_size builder;
      Llvm.build_ret new_heap builder;
      block
    in
    Llvm.build_cond_br enough_space return extend_space builder

  let fill_gc_init builder =
    let initial_heap_size = Llvm.const_int Type.i32 I.initial_heap_size in
    let new_heap = Llvm.build_call malloc [|initial_heap_size|] "" builder in
    (* TODO: Handle malloc failures *)
    Llvm.build_store new_heap gc_heap builder;
    Llvm.build_store initial_heap_size gc_heap_size builder;
    Llvm.build_ret_void builder

  let create_builtin_instruction ty name g =
    let ty = Llvm.function_type ty [|ty; ty|] in
    let (f, builder) = Llvm.define_function `External c (fmt ".labrys.%s" name) ty m in
    let v = g (Llvm.param f 0) (Llvm.param f 1) "" builder in
    Llvm.build_ret v builder

  let () = begin
    create_builtin_instruction Type.i32 "add" Llvm.build_add;
    create_builtin_instruction Type.i32 "mul" Llvm.build_mul;
  end

  let main_init =
    let ty = Llvm.function_type Type.void [|Type.jmp_buf_ptr|] in
    Llvm.declare_function (Generic.init_name I.main_module) ty m

  let init_gc builder =
    let (_, gc_builder) = Llvm.define_function `External c "GC_malloc" Generic.malloc_type m in
    fill_gc_malloc gc_builder;
    let (gc_init, gc_builder) = Llvm.define_function `External c "GC_init" Type.unit_function m in
    fill_gc_init gc_builder;
    Llvm.build_call_void gc_init [||] builder

  let make () =
    let (_, builder) =
      Llvm.define_function `External c "main" Type.main_function m
    in
    init_gc builder;
    let jmp_buf = Generic.alloc_jmp_buf builder in
    Llvm.build_call_void main_init [|jmp_buf|] builder;
    Llvm.build_ret (i32 0) builder;
    m
end

module Make (I : I) = struct
  type env =
    | Value of Llvm.llvalue
    | Env of int
    | Global of Llvm.llvalue

  let m = Llvm.create_module c (Module.to_string I.name)

  module Generic = Generic (struct let m = m end)

  let build_gc_malloc ty name builder =
    let size = Llvm.size_of ty in
    let size = Llvm.const_trunc_or_bitcast size Type.i32 in
    Llvm.build_call Generic.gc_malloc [|size|] name builder

  let init ptr ty values builder =
    let aux acc i x = Llvm.build_insertvalue acc x i "" builder in
    let values = List.foldi aux (Llvm.undef ty) values in
    Llvm.build_store values ptr builder

  let malloc_and_init size values builder =
    let ty = Type.array size in
    let allocated = build_gc_malloc ty "" builder in
    init allocated ty values builder;
    Llvm.build_bitcast allocated Type.star "" builder

  let malloc_and_init values builder =
    match List.length values with
    | 0 -> null
    | size -> malloc_and_init size values builder

  let debug_trap = Llvm.declare_function "llvm.debugtrap" Type.unit_function m

  let load_rt_env builder =
    let rt_env = Llvm.current_param builder 1 in
    Llvm.build_load rt_env "" builder

  let unreachable builder =
    if I.debug then
      Llvm.build_call_void debug_trap [||] builder;
    Llvm.build_unreachable builder

  let const_int_to_star x =
    Llvm.const_inttoptr (Llvm.const_int Type.i32 x) Type.star

  let longjmp =
    let ty = Llvm.function_type Type.void [|Type.star|] in
    Llvm.declare_function "llvm.eh.sjlj.longjmp" ty m

  let setjmp =
    let ty = Llvm.function_type Type.i32 [|Type.star|] in
    Llvm.declare_function "llvm.eh.sjlj.setjmp" ty m

  let exn_var =
    let v = Llvm.define_global "exn" (Llvm.undef Type.exn_glob) m in
    Llvm.set_thread_local true v;
    Llvm.set_linkage Llvm.Linkage.Link_once_odr v;
    v

  let fold_rt_env env builder =
    let aux name value (i, values, env) =
      match value with
      | Value value ->
          let values = value :: values in
          let env = LIdent.Map.add name (Env i) env in
          (succ i, values, env)
      | Env j ->
          let rt_env = load_rt_env builder in
          let value = Llvm.build_extractvalue rt_env j "" builder in
          let values = value :: values in
          let env = LIdent.Map.add name (Env i) env in
          (succ i, values, env)
      | Global value ->
          let env = LIdent.Map.add name (Global value) env in
          (i, values, env)
    in
    let (_, b, c) = LIdent.Map.fold aux env (1, [], LIdent.Map.empty) in
    (List.rev b, c)

  let create_closure ~free_vars env builder =
    let env = LIdent.Map.filter (fun x _ -> Set.mem free_vars x) env in
    let (values, env) = fold_rt_env env builder in
    let rt_env_size = List.length values in
    let rt_env_size = succ rt_env_size in
    let (f, builder') = Llvm.define_function `Private c "__lambda" (Type.lambda ~rt_env_size) m in
    Llvm.set_linkage Llvm.Linkage.Private f;
    let f = Llvm.build_bitcast f Type.star "" builder in
    let closure = malloc_and_init (f :: values) builder in
    (builder', closure, env)

  let get_exn name =
    let name = LIdent.to_string name in
    Llvm.declare_global Type.i8 name m

  let get_const = function
    | `Int n -> Llvm.const_int Type.i32 n
    | `Float n -> Llvm.const_float Type.float n
    | `Char n -> Llvm.const_int Type.i32 (Uchar.to_int n)
    | `String s -> Llvm.const_string c (s ^ "\x00")

  let llvm_ty_of_ty = function
    | `Int () -> Type.i32
    | `Float () -> Type.float
    | `Char () -> Type.i32
    | `String () -> Type.star
    | `Custom -> Type.star
    | `Void -> Type.void

  let args_type args =
    let aux (ty, _) = llvm_ty_of_ty ty in
    Array.of_list (List.map aux args)

  let create_fail jmp_buf builder =
    let jmp_buf = Llvm.build_bitcast jmp_buf Type.star "" builder in
    Llvm.build_call_void longjmp [|jmp_buf|] builder;
    unreachable builder;
    (undef, snd (Llvm.create_block c builder))

  let get_value env builder name =
    match LIdent.Map.find_opt name env with
    | Some (Value value) ->
        value
    | Some (Env i) ->
        let rt_env = load_rt_env builder in
        Llvm.build_extractvalue rt_env i "" builder
    | Some (Global value) ->
        Llvm.build_load value "" builder
    | None ->
        let name = LIdent.to_string name in
        let extern = Llvm.declare_global Type.star name m in
        Llvm.build_load extern "" builder

  let alloc_vars builder =
    let aux var = (var, Llvm.build_alloca Type.star "" builder) in
    List.map aux

  let load_vars builder =
    let aux env (name, var) =
      let var = Llvm.build_load var "" builder in
      LIdent.Map.add name (Value var) env
    in
    List.fold_left aux

  let map_args env builder = function
    | [] ->
        [||]
    | args ->
        let aux = function
          | (`Custom, name) ->
              get_value env builder name
          | (ty, name) ->
              let ty = Llvm.pointer_type (llvm_ty_of_ty ty) in
              let v = get_value env builder name in
              Llvm.build_load_cast v ty builder
        in
        Array.of_list (List.map aux args)

  let extract_constr_args term len builder =
    List.init len (fun i -> Llvm.build_extractvalue term (succ i) "" builder)

  let rec build_if_chain ~default vars env builder values term results cases =
    let aux builder (constr, len, tree) =
      let if_branch =
        let (block, builder) = Llvm.create_block c builder in
        let term = try List.hd values with Failure _ -> assert false in
        let term = Llvm.build_load_cast term (Type.variant_ptr (succ len)) builder in
        let values = try List.tl values with Failure _ -> assert false in
        let values = extract_constr_args term len builder @ values in
        create_tree vars env builder values results tree;
        block
      in
      let (else_branch, else_builder) = Llvm.create_block c builder in
      let constr = match constr with
        | OptimizedTree.Index constr -> const_int_to_star constr
        | OptimizedTree.Exn exn -> get_exn exn
      in
      let cmp = Llvm.build_icmp Llvm.Icmp.Eq term constr "" builder in
      Llvm.build_cond_br cmp if_branch else_branch builder;
      else_builder
    in
    let builder = List.fold_left aux builder cases in
    Llvm.build_br default builder

  and create_tree vars env builder values results = function
    | OptimizedTree.Jump branch ->
        let (block, _) = List.nth results branch in
        Llvm.build_br block builder
    | OptimizedTree.Switch (cases, default) ->
        let term = try List.hd values with Failure _ -> assert false in
        let term = Llvm.build_load_cast term (Type.variant_ptr 1) builder in
        let term = Llvm.build_extractvalue term 0 "" builder in
        let default =
          let (block, builder) = Llvm.create_block c builder in
          let values = try List.tl values with Failure _ -> assert false in
          create_tree vars env builder values results default;
          block
        in
        build_if_chain ~default vars env builder values term results cases
    | OptimizedTree.Alias (name, p) ->
        let name = try List.assoc ~eq:LIdent.equal name vars with Not_found -> assert false in
        let term = try List.hd values with Failure _ -> assert false in
        Llvm.build_store term name builder;
        create_tree vars env builder values results p
    | OptimizedTree.Swap (idx, p) ->
        let values = Utils.swap_list idx values in
        create_tree vars env builder values results p

  let map_ret builder t = function
    | `Void ->
        let v = Llvm.define_constant "" (Llvm.const_array Type.star [||]) m in
        let v = Llvm.const_bitcast v Type.star in
        (v, builder)
    | `Custom ->
        (t, builder)
    | ty ->
        let ty = llvm_ty_of_ty ty in
        let value = build_gc_malloc ty "" builder in
        Llvm.build_store t value builder;
        let value = Llvm.build_bitcast value Type.star "" builder in
        (value, builder)

  let rec create_results ~last_bind ~jmp_buf ~next_block vars env builder =
    let aux (env, results) result =
      let (block, builder') = Llvm.create_block c builder in
      let env = load_vars builder' env vars in
      let (v, builder'') = lambda ~last_bind ~jmp_buf env builder' result in
      Llvm.build_br next_block builder'';
      (env, results @ [(block, (v, Llvm.insertion_block builder''))])
    in
    List.fold_left aux (env, [])

  and abs ~last_bind ~name t env builder =
    let param = Llvm.current_param builder 0 in
    let jmp_buf = Llvm.current_param builder 2 in
    let env = match last_bind with
      | (name, _) when LIdent.Map.mem name env -> env
      | (name, Some v) -> LIdent.Map.add name (Global v) env
      | (name, None) ->
          let rec_value = Llvm.current_param builder 1 in
          let rec_value = Llvm.build_bitcast rec_value Type.star "" builder in
          LIdent.Map.add name (Value rec_value) env
    in
    let env = LIdent.Map.add name (Value param) env in
    let (v, builder) = lambda ~last_bind ~jmp_buf env builder t in
    Llvm.build_ret v builder

  and lambda' ~last_bind ~jmp_buf env builder = function
    | OptimizedTree.Abs (name, free_vars, t) ->
        let (builder', closure, env) =
          create_closure ~free_vars env builder
        in
        abs ~last_bind ~name t env builder';
        (closure, builder)
    | OptimizedTree.App (f, x) ->
        let closure = get_value env builder f in
        let x = get_value env builder x in
        let closure = Llvm.build_bitcast closure (Type.closure_ptr 1) "" builder in
        let f = Llvm.build_load closure "" builder in
        let f = Llvm.build_extractvalue f 0 "" builder in
        let f = Llvm.build_bitcast f (Type.lambda_ptr ~rt_env_size:1) "" builder in
        (Llvm.build_call f [|x; closure; jmp_buf|] "" builder, builder)
    | OptimizedTree.PatternMatching (t, vars, results, tree) ->
        let t = get_value env builder t in
        let (next_block, next_builder) = Llvm.create_block c builder in
        let vars = alloc_vars builder vars in
        let (env, results) = create_results ~last_bind ~next_block ~jmp_buf vars env builder results in
        create_tree vars env builder [t] results tree;
        let results = List.map snd results in
        (Llvm.build_phi results "" next_builder, next_builder)
    | OptimizedTree.Val name ->
        (get_value env builder name, builder)
    | OptimizedTree.Datatype (index, params) ->
        let values = List.map (get_value env builder) params in
        let values = match index with
          | Some (OptimizedTree.Index index) ->
              Llvm.const_inttoptr (i32 index) Type.star :: values
          | Some (OptimizedTree.Exn name) ->
              get_exn name :: values
          | None ->
              values
        in
        if List.for_all Llvm.is_constant values then begin
          let v = Array.of_list values in
          let v = Llvm.define_constant "" (Llvm.const_array Type.star v) m in
          let v = Llvm.const_bitcast v Type.star in
          (v, builder)
        end else begin
          let v = malloc_and_init values builder in
          (v, builder)
        end
    | OptimizedTree.CallForeign (name, ret, args) ->
        let ty = Llvm.function_type (llvm_ty_of_ty ret) (args_type args) in
        let f = Llvm.declare_function name ty m in
        let args = map_args env builder args in
        map_ret builder (Llvm.build_call f args "" builder) ret
    | OptimizedTree.Fail name ->
        let v = get_value env builder name in
        Llvm.build_store v exn_var builder;
        create_fail jmp_buf builder
    | OptimizedTree.Try (t, (name, t')) ->
        let jmp_buf' = Generic.alloc_jmp_buf builder in
        let (next_block, next_builder) = Llvm.create_block c builder in
        let jmp_buf_gen = Llvm.build_bitcast jmp_buf' Type.star "" builder in
        let jmp_res = Llvm.build_call setjmp [|jmp_buf_gen|] "" builder in
        let cond = Llvm.build_icmp Llvm.Icmp.Eq jmp_res (i32 0) "" builder in
        let (try_result, try_block) =
          let (block, builder) = Llvm.create_block c builder in
          let (t, builder') = lambda ~last_bind ~jmp_buf:jmp_buf' env builder t in
          Llvm.build_br next_block builder';
          ((t, Llvm.insertion_block builder'), block)
        in
        let (catch_result, catch_block) =
          let (block, builder) = Llvm.create_block c builder in
          let exn = Llvm.build_load exn_var "" builder in
          Llvm.build_store (Llvm.undef Type.exn_glob) exn_var builder;
          let env = LIdent.Map.add name (Value exn) env in
          let (t', builder') = lambda ~last_bind ~jmp_buf env builder t' in
          Llvm.build_br next_block builder';
          ((t', Llvm.insertion_block builder'), block)
        in
        Llvm.build_cond_br cond try_block catch_block builder;
        (Llvm.build_phi [try_result; catch_result] "" next_builder, next_builder)
    | OptimizedTree.RecordGet (name, n) ->
        let t = get_value env builder name in
        let t = Llvm.build_load_cast t (Type.array_ptr (succ n)) builder in
        (Llvm.build_extractvalue t n "" builder, builder)
    | OptimizedTree.Const const ->
        let v = Llvm.define_constant "" (get_const const) m in
        let v = Llvm.const_bitcast v Type.star in
        (v, builder)
    | OptimizedTree.Unreachable ->
        unreachable builder;
        (undef, snd (Llvm.create_block c builder))

  and lambda ~last_bind ~jmp_buf env builder (lets, t) =
    let rec aux env builder = function
      | (name, x)::xs ->
          let last_bind = (name, None) in
          let (t, builder) = lambda' ~last_bind ~jmp_buf env builder x in
          let env = LIdent.Map.add name (Value t) env in
          aux env builder xs
      | [] ->
          (env, builder)
    in
    let (env, builder) = aux env builder lets in
    lambda' ~last_bind ~jmp_buf env builder t

  let set_linkage v = function
    | OptimizedTree.Private -> Llvm.set_linkage Llvm.Linkage.Private v
    | OptimizedTree.Public -> Llvm.set_linkage Llvm.Linkage.External v

  let define_global ~name ~linkage value =
    let name = LIdent.to_string name in
    let name' = "." ^ name in
    let v = Llvm.define_constant name' value m in
    let v = Llvm.define_constant name (Llvm.const_bitcast v Type.star) m in
    set_linkage v linkage;
    v

  let init_imports ~jmp_buf imports builder =
    let aux import =
      let f = Llvm.declare_global Type.init (Generic.init_name import) m in
      Llvm.build_call_void f [|jmp_buf|] builder
    in
    List.iter aux imports

  let make ~imports l =
    let aux ~jmp_buf env builder = function
      | OptimizedTree.Value (name, t, linkage) ->
          let name' = LIdent.to_string name in
          let global = Llvm.define_global name' null m in
          set_linkage global linkage;
          let last_bind = (name, Some global) in
          let (value, builder) = lambda ~last_bind ~jmp_buf env builder t in
          Llvm.build_store value global builder;
          builder
      | OptimizedTree.Exception name ->
          let name = LIdent.to_string name in
          (* NOTE: Don't use Llvm.define_constant as it merges equal values *)
          let v = Llvm.define_global name (string name) m in
          Llvm.set_global_constant true v;
          builder
      | OptimizedTree.Function (name, (name', t), linkage) ->
          let (f, builder') = Llvm.define_function `Private c (".." ^ LIdent.to_string name) (Type.lambda ~rt_env_size:0) m in
          let f = define_global ~name ~linkage (Llvm.const_array Type.star [|Llvm.const_bitcast f Type.star|]) in
          let last_bind = (name, Some f) in
          abs ~last_bind ~name:name' t env builder';
          builder
    in
    let (f, builder) =
      Llvm.define_function `External c (Generic.init_name I.name) Type.init m
    in
    let jmp_buf = Llvm.param f 0 in
    init_imports ~jmp_buf imports builder;
    let builder = List.fold_left (aux ~jmp_buf LIdent.Map.empty) builder l in
    Llvm.build_ret_void builder;
    m
end

let make ~modul ~imports options x =
  let module Module =
    Make(struct
      let name = modul
      let debug = options#debug
    end)
  in
  Module.make ~imports x

let main options main_module =
  let module Module =
    Main(struct
      let initial_heap_size = options#initial_heap_size
      let main_module = main_module
    end)
  in
  Module.make ()

let link options ~main_module_name ~main_module imports =
  let aux _ x dst =
    Llvm_linker.link_modules' dst x;
    dst
  in
  let dst = main_module in
  let () =
    let src = main options main_module_name in
    Llvm_linker.link_modules' dst src;
  in
  Module.Map.fold aux imports dst

let init = lazy (Llvm_all_backends.initialize ())

let get_triple () =
  Lazy.force init;
  Llvm_target.Target.default_triple ()

let get_target ~triple =
  let target = Llvm_target.Target.by_triple triple in
  let reloc_mode = Llvm_target.RelocMode.PIC in
  Llvm_target.TargetMachine.create ~triple ~reloc_mode target

let privatize_identifiers m =
  let aux f v =
    if not (Llvm.is_declaration v || f (Llvm.value_name v)) then
      Llvm.set_linkage Llvm.Linkage.Private v;
  in
  Llvm.iter_globals (aux (fun _ -> false)) m;
  Llvm.iter_functions (aux (String.equal "main")) m

let optimize options m =
  privatize_identifiers m;
  let lto = options#lto in
  let opt = options#opt in
  let triple = get_triple () in
  let target = get_target ~triple in
  let layout = Llvm_target.TargetMachine.data_layout target in
  let layout = Llvm_target.DataLayout.as_string layout in
  Llvm.set_target_triple triple m;
  Llvm.set_data_layout layout m;
  Llvm.optimize ~lto ~opt target m;
  m

let to_string = Llvm.string_of_llmodule

let write_bitcode ~o m =
  Utils.mkdir o;
  if not (Llvm_bitwriter.write_bitcode_file m o) then
    Err.fail_module "File '%s' cannot be created" o

let read_bitcode file =
  try
    let buf = Llvm.MemoryBuffer.of_file file in
    Llvm_bitreader.parse_bitcode c buf
  with
  | _ -> raise BuildSystem.Failure

let emit_object_file ~tmp m =
  let triple = get_triple () in
  let target = get_target ~triple in
  Llvm_target.TargetMachine.emit_to_file
    m
    Llvm_target.CodeGenFileType.ObjectFile
    tmp
    target

let default_heap_size = 4096
