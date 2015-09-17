(*
Copyright (c) 2013-2015 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

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

open Monomorphic_containers.Open

module Set = GammaSet.Value
module Map = struct
  include Map.Make (struct
      type t = PatternMatrix.var

      let compare = PatternMatrix.var_compare
    end)
end

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
  let star = Llvm.pointer_type i8
  let array = Llvm.array_type star
  let array_ptr size = Llvm.pointer_type (array size)
  let variant = array
  let variant_ptr = array_ptr
  let closure = array
  let closure_ptr = array_ptr

  (** Note: jmp_buf is a five word buffer (see the Llvm doc). *)
  let jmp_buf = Llvm.array_type star 5
  let jmp_buf_ptr = Llvm.pointer_type jmp_buf
  let lambda ~env_size =
    Llvm.function_type star [|star; closure_ptr env_size; jmp_buf_ptr|]
  let lambda_ptr ~env_size =
    Llvm.pointer_type (lambda ~env_size)
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

  let init_jmp_buf jmp_buf builder =
    let v = Llvm.undef Type.jmp_buf in
    let fp = Llvm.build_call frameaddress [|i32 0|] "" builder in
    let v = Llvm.build_insertvalue v fp 0 "" builder in
    let sp = Llvm.build_call stacksave [||] "" builder in
    let v = Llvm.build_insertvalue v sp 2 "" builder in
    Llvm.build_store v jmp_buf builder
end

module Main (I : sig val main_module : Module.t end) = struct
  let m = Llvm.create_module c "_main_"

  module Generic = Generic (struct let m = m end)

  let main_init =
    let ty = Llvm.function_type Type.void [|Type.jmp_buf_ptr|] in
    Llvm.declare_function (Generic.init_name I.main_module) ty m

  let init_gc builder =
    let gc_init = Llvm.declare_function "GC_init" Type.unit_function m in
    Llvm.build_call_void gc_init [||] builder

  let make () =
    let (_, builder) = Llvm.define_function c "main" Type.main_function m in
    init_gc builder;
    let jmp_buf = Llvm.build_alloca Type.jmp_buf "" builder in
    Generic.init_jmp_buf jmp_buf builder;
    Llvm.build_call_void main_init [|jmp_buf|] builder;
    Llvm.build_ret (i32 0) builder;
    m
end

module Make (I : I) = struct
  type gamma =
    | Value of Llvm.llvalue
    | Env of int
    | RecFun
    | Global of Llvm.llvalue

  let m = Llvm.create_module c (Module.to_string I.name)

  module Generic = Generic (struct let m = m end)

  let init ptr ty values builder =
    let aux acc i x = Llvm.build_insertvalue acc x i "" builder in
    let values = List.Idx.foldi aux (Llvm.undef ty) values in
    Llvm.build_store values ptr builder

  let malloc_and_init ty values builder =
    let allocated = Llvm.build_malloc ty "" builder in
    init allocated ty values builder;
    allocated

  let malloc_and_init_array size values builder =
    match size with
    | 0 -> null
    | size -> malloc_and_init (Type.array size) values builder

  let debug_trap = Llvm.declare_function "llvm.debugtrap" Type.unit_function m

  let load_env func builder =
    let env = Llvm.param func 1 in
    Llvm.build_load env "" builder

  let unreachable builder =
    if I.debug then
      Llvm.build_call_void debug_trap [||] builder;
    Llvm.build_unreachable builder

  let longjmp =
    let ty = Llvm.function_type Type.void [|Type.star|] in
    Llvm.declare_function "llvm.eh.sjlj.longjmp" ty m

  let setjmp =
    let ty = Llvm.function_type Type.i32 [|Type.star|] in
    Llvm.declare_function "llvm.eh.sjlj.setjmp" ty m

  let exn_tag_var =
    let v = Llvm.define_global "exn_tag" null m in
    Llvm.set_thread_local true v;
    Llvm.set_linkage Llvm.Linkage.Link_once_odr v;
    v

  let exn_args_var =
    let v = Llvm.define_global "exn_args" null m in
    Llvm.set_thread_local true v;
    Llvm.set_linkage Llvm.Linkage.Link_once_odr v;
    v

  let create_default_branch func =
    let block = Llvm.append_block c "" func in
    let builder = Llvm.builder_at_end c block in
    unreachable builder;
    block

  let fold_env func gamma builder =
    let aux name value (i, values, gamma) =
      match value with
      | Value value ->
          let values = value :: values in
          let gamma = GammaMap.Value.add name (Env i) gamma in
          (succ i, values, gamma)
      | Env j ->
          let env = load_env func builder in
          let value = Llvm.build_extractvalue env j "" builder in
          let values = value :: values in
          let gamma = GammaMap.Value.add name (Env i) gamma in
          (succ i, values, gamma)
      | RecFun ->
          let value = Llvm.param func 1 in
          let value = Llvm.build_bitcast value Type.star "" builder in
          let values = value :: values in
          let gamma = GammaMap.Value.add name (Env i) gamma in
          (succ i, values, gamma)
      | Global value ->
          let gamma = GammaMap.Value.add name (Global value) gamma in
          (i, values, gamma)
    in
    let (_, b, c) = GammaMap.Value.fold aux gamma (1, [], GammaMap.Value.empty) in
    (List.rev b, c)

  let create_closure func ~isrec ~used_vars gamma builder =
    let gamma = GammaMap.Value.filter (fun x _ -> Set.mem x used_vars) gamma in
    let (values, gamma) = fold_env func gamma builder in
    let env_size = List.length values in
    let gamma = match isrec with
      | Some rec_name when Set.mem rec_name used_vars ->
          GammaMap.Value.add rec_name RecFun gamma
      | Some _ | None ->
          gamma
    in
    let env_size = succ env_size in
    let (f, builder') = Llvm.define_function c "__lambda" (Type.lambda ~env_size) m in
    Llvm.set_linkage Llvm.Linkage.Private f;
    let f' = Llvm.build_bitcast f Type.star "" builder in
    let closure = malloc_and_init (Type.closure env_size) (f' :: values) builder in
    (f, builder', closure, gamma)

  let get_exn name =
    let name = Ident.Exn.to_string name in
    Llvm.declare_global Type.i8 name m

  let rec llvalue_of_pattern_var vars value builder var =
    match Map.find var vars with
    | value ->
        (value, vars)
    | exception Not_found ->
        let (value, vars) =
          match var with
          | Pattern.VLeaf ->
              (value, vars)
          | Pattern.VNode (i, var) ->
              let i = succ i in
              let (value, vars) = llvalue_of_pattern_var vars value builder var in
              let value = Llvm.build_bitcast value (Type.variant_ptr (succ i)) "" builder in
              let value = Llvm.build_load value "" builder in
              (Llvm.build_extractvalue value i "" builder, vars)
        in
        (value, Map.add var value vars)

  let rec create_branch func ~default vars gamma value results tree =
    let block = Llvm.append_block c "" func in
    let builder = Llvm.builder_at_end c block in
    create_tree func ~default vars gamma builder value results tree;
    block

  and create_result func ~jmp_buf ~next_block gamma builder (vars, result) =
    let block = Llvm.append_block c "" func in
    let builder' = Llvm.builder_at_end c block in
    let (gamma, pattern_vars) =
      let aux (gamma, pattern_vars) (var, name) =
        let variable = Llvm.build_alloca Type.star "" builder in
        let value = Llvm.build_load variable "" builder' in
        (GammaMap.Value.add name (Value value) gamma, (var, variable) :: pattern_vars)
      in
      List.fold_left aux (gamma, []) vars
    in
    let (v, builder') = lambda func ~jmp_buf gamma builder' result in
    Llvm.build_br next_block builder';
    (block, (v, Llvm.insertion_block builder'), pattern_vars)

  and create_tree func ~default vars gamma builder value results = function
    | UntypedTree.Leaf i ->
        let (block, _, pattern_vars) = List.nth results i in
        let aux vars (var, variable) =
          let (var, vars) = llvalue_of_pattern_var vars value builder var in
          Llvm.build_store var variable builder;
          vars
        in
        ignore (List.fold_left aux vars pattern_vars);
        Llvm.build_br block builder
    | UntypedTree.Node (var, cases) ->
        let (term, vars) = llvalue_of_pattern_var vars value builder var in
        let term = Llvm.build_bitcast term (Type.variant_ptr 1) "" builder in
        let term = Llvm.build_load term "" builder in
        let term = Llvm.build_extractvalue term 0 "" builder in
        let term = Llvm.build_ptrtoint term Type.i32 "" builder in
        let switch = Llvm.build_switch term default (List.length cases) builder in
        List.iter
          (fun (constr, tree) ->
             let branch = create_branch func ~default vars gamma value results tree in
             Llvm.add_case switch (i32 constr) branch
          )
          cases

  and create_exn_result func ~jmp_buf ~next_block ~exn_args gamma (args, result) =
    let block = Llvm.append_block c "" func in
    let builder = Llvm.builder_at_end c block in
    let exn_args = Llvm.build_bitcast exn_args (Type.array_ptr (List.length args)) "" builder in
    let exn_args = lazy (Llvm.build_load exn_args "" builder) in
    let gamma =
      let aux gamma i name =
        let exn_args = Lazy.force exn_args in
        let value = Llvm.build_extractvalue exn_args i "" builder in
        GammaMap.Value.add name (Value value) gamma
      in
      List.Idx.foldi aux gamma args
    in
    let (v, builder) = lambda func ~jmp_buf gamma builder result in
    Llvm.build_br next_block builder;
    ((v, Llvm.insertion_block builder), block)

  and create_exn_branches func ~jmp_buf ~next_block gamma builder branches =
    let exn_tag = Llvm.build_load exn_tag_var "" builder in
    let exn_args = Llvm.build_load exn_args_var "" builder in
    let aux (acc, builder) ((name, args), t) =
      let (x, block) = create_exn_result func ~next_block ~jmp_buf ~exn_args gamma (args, t) in
      let exn = get_exn name in
      let next_block = Llvm.append_block c "" func in
      let cond = Llvm.build_icmp Llvm.Icmp.Eq exn exn_tag "" builder in
      Llvm.build_cond_br cond block next_block builder;
      (x :: acc, Llvm.builder_at_end c next_block)
    in
    let (x, builder) = List.fold_left aux ([], builder) branches in
    let jmp_buf = Llvm.build_bitcast jmp_buf Type.star "" builder in
    Llvm.build_call_void longjmp [|jmp_buf|] builder;
    unreachable builder;
    x

  and abs ~f ~name t gamma builder =
    let param = Llvm.param f 0 in
    let jmp_buf = Llvm.param f 2 in
    let gamma = GammaMap.Value.add name (Value param) gamma in
    let (v, builder) = lambda f ~jmp_buf gamma builder t in
    Llvm.build_ret v builder

  and lambda func ?isrec ~jmp_buf gamma builder = function
    | UntypedTree.Abs (name, used_vars, t) ->
        let (f, builder', closure, gamma) =
          create_closure func ~isrec ~used_vars gamma builder
        in
        abs ~f ~name t gamma builder';
        let closure = Llvm.build_bitcast closure Type.star "" builder in
        (closure, builder)
    | UntypedTree.App (f, x) ->
        let (closure, builder) = lambda func ~jmp_buf gamma builder f in
        let (x, builder) = lambda func ~jmp_buf gamma builder x in
        let closure = Llvm.build_bitcast closure (Type.closure_ptr 1) "" builder in
        let f = Llvm.build_load closure "" builder in
        let f = Llvm.build_extractvalue f 0 "" builder in
        let f = Llvm.build_bitcast f (Type.lambda_ptr ~env_size:1) "" builder in
        (Llvm.build_call f [|x; closure; jmp_buf|] "" builder, builder)
    | UntypedTree.PatternMatching (t, results, tree) ->
        let (t, builder) = lambda func ~jmp_buf gamma builder t in
        let next_block = Llvm.append_block c "" func in
        let results = List.map (create_result func ~next_block ~jmp_buf gamma builder) results in
        let builder' = Llvm.builder_at_end c next_block in
        let default = create_default_branch func in
        create_tree func ~default Map.empty gamma builder t results tree;
        let results = List.map (fun (_, x, _) -> x) results in
        (Llvm.build_phi results "" builder', builder')
    | UntypedTree.Val name ->
        begin match GammaMap.Value.find_opt name gamma with
        | Some (Global _) ->
            (* Unused for the moment *)
            assert false
        | Some (Value value) ->
            (value, builder)
        | Some (Env i) ->
            let env = load_env func builder in
            (Llvm.build_extractvalue env i "" builder, builder)
        | Some RecFun ->
            let value = Llvm.param func 1 in
            let value = Llvm.build_bitcast value Type.star "" builder in
            (value, builder)
        | None ->
            let name = Ident.Name.to_string name in
            let extern = Llvm.declare_global Type.star name m in
            (Llvm.build_load extern "" builder, builder)
        end
    | UntypedTree.Variant (i, params) ->
        let aux x =
          match GammaMap.Value.find_opt x gamma with
          | Some (Value x) -> x
          | Some (Env i) ->
              let env = load_env func builder in
              Llvm.build_extractvalue env i "" builder
          | Some RecFun
          | Some (Global _)
          | None ->
              assert false
        in
        let values = List.map aux params in
        begin match List.length values with
        | 0 ->
            let index = Llvm.const_inttoptr (i32 i) Type.star in
            let v = Llvm.define_global "" (Llvm.const_array Type.star [|index|]) m in
            Llvm.set_linkage Llvm.Linkage.Private v;
            Llvm.set_global_constant true v;
            let v = Llvm.build_bitcast v Type.star "" builder in
            (v, builder)
        | size ->
            let i = Llvm.build_inttoptr (i32 i) Type.star "" builder in
            let value = malloc_and_init (Type.variant (succ size)) (i :: values) builder in
            let value = Llvm.build_bitcast value Type.star "" builder in
            (value, builder)
        end
    | UntypedTree.Call (name, args) ->
        let f =
          match GammaMap.Value.find_opt name gamma with
          | Some (Global value) -> value
          | Some (Value _) | Some (Env _) | Some RecFun | None -> assert false
        in
        let (args, builder) = fold_args func ~jmp_buf gamma builder args in
        let args = Array.of_list args in
        let ty = Llvm.function_type Type.star (Array.map (fun _ -> Type.star) args) in
        let f = Llvm.build_bitcast f (Llvm.pointer_type ty) "" builder in
        (Llvm.build_call f args "" builder, builder)
    | UntypedTree.Let (name, t, xs) ->
        let (t, builder) = lambda func ~jmp_buf gamma builder t in
        let gamma = GammaMap.Value.add name (Value t) gamma in
        lambda func ~jmp_buf gamma builder xs
    | UntypedTree.LetRec (name, t, xs) ->
        let (t, builder) = lambda func ~isrec:name ~jmp_buf gamma builder t in
        let gamma = GammaMap.Value.add name (Value t) gamma in
        lambda func ~jmp_buf gamma builder xs
    | UntypedTree.Fail (name, args) ->
        let (args, builder) = fold_args func ~jmp_buf gamma builder args in
        let args = malloc_and_init_array (List.length args) args builder in
        let tag = get_exn name in
        Llvm.build_store args exn_args_var builder;
        Llvm.build_store tag exn_tag_var builder;
        let jmp_buf = Llvm.build_bitcast jmp_buf Type.star "" builder in
        Llvm.build_call_void longjmp [|jmp_buf|] builder;
        unreachable builder;
        (undef, Llvm.builder_at_end c (Llvm.append_block c "" func))
    | UntypedTree.Try (t, branches) ->
        let jmp_buf' = Llvm.build_alloca Type.jmp_buf "" builder in
        Generic.init_jmp_buf jmp_buf' builder;
        let next_block = Llvm.append_block c "" func in
        let (try_result, try_block) =
          let block = Llvm.append_block c "" func in
          let builder = Llvm.builder_at_end c block in
          let (t, builder) = lambda func ~jmp_buf:jmp_buf' gamma builder t in
          Llvm.build_br next_block builder;
          ((t, Llvm.insertion_block builder), block)
        in
        let (results, catch_block) =
          let block = Llvm.append_block c "" func in
          let builder = Llvm.builder_at_end c block in
          let results = create_exn_branches func ~jmp_buf ~next_block gamma builder branches in
          (try_result :: results, block)
        in
        let jmp_buf' = Llvm.build_bitcast jmp_buf' Type.star "" builder in
        let jmp_res = Llvm.build_call setjmp [|jmp_buf'|] "" builder in
        let cond = Llvm.build_icmp Llvm.Icmp.Eq jmp_res (i32 0) "" builder in
        Llvm.build_cond_br cond try_block catch_block builder;
        let builder = Llvm.builder_at_end c next_block in
        (Llvm.build_phi results "" builder, builder)
    | UntypedTree.RecordGet (t, n) ->
        let (t, builder) = lambda func ~jmp_buf gamma builder t in
        let t = Llvm.build_bitcast t (Type.array_ptr (succ n)) "" builder in
        let t = Llvm.build_load t "" builder in
        (Llvm.build_extractvalue t n "" builder, builder)
    | UntypedTree.RecordCreate fields ->
        let (fields, builder) = fold_args func ~jmp_buf gamma builder fields in
        let record = malloc_and_init_array (List.length fields) fields builder in
        let record = Llvm.build_bitcast record Type.star "" builder in
        (record, builder)

  and fold_args func ~jmp_buf gamma builder args =
    let aux (acc, builder) x =
      let (x, builder) = lambda func ~jmp_buf gamma builder x in
      (x :: acc, builder)
    in
    let (args, builder) = List.fold_left aux ([], builder) args in
    (List.rev args, builder)

  let set_linkage v = function
    | UntypedTree.Private -> Llvm.set_linkage Llvm.Linkage.Private v
    | UntypedTree.Public -> ()

  let define_global ~name ~linkage value =
    let name = Ident.Name.to_string name in
    let name' = "." ^ name in
    let v = Llvm.define_global name' value m in
    Llvm.set_linkage Llvm.Linkage.Private v;
    Llvm.set_global_constant true v;
    let v = Llvm.define_global name (Llvm.const_bitcast v Type.star) m in
    set_linkage v linkage;
    Llvm.set_global_constant true v

  let rec init func ~jmp_buf bindings builder = function
    | `Val (global, t) :: xs ->
        let (value, builder) = lambda func ~jmp_buf bindings builder t in
        Llvm.build_store value global builder;
        init func ~jmp_buf bindings builder xs
    | `Const g :: xs ->
        g bindings;
        init func ~jmp_buf bindings builder xs
    | `Binding (name, v) :: xs ->
        let bindings = GammaMap.Value.add name (Global v) bindings in
        init func ~jmp_buf bindings builder xs
    | [] ->
        builder

  let init func builder =
    let jmp_buf = Llvm.build_alloca Type.jmp_buf "" builder in
    Generic.init_jmp_buf jmp_buf builder;
    init func ~jmp_buf GammaMap.Value.empty builder

  let () =
    let malloc_type = (Llvm.function_type Type.star [|Type.i32|]) in
    let (malloc, builder) = Llvm.define_function c "malloc" malloc_type m in
    Llvm.set_linkage Llvm.Linkage.Private malloc;
    let gc_malloc = Llvm.declare_function "GC_malloc" malloc_type m in
    Llvm.build_ret (Llvm.build_call gc_malloc (Llvm.params malloc) "" builder) builder

  let init_imports ~jmp_buf imports builder =
    let aux import =
      let f = Llvm.declare_global Type.init (Generic.init_name import) m in
      Llvm.build_call_void f [|jmp_buf|] builder
    in
    List.iter aux imports

  let make ~imports =
    let rec top init_list = function
      | UntypedTree.Value (name, t, linkage) :: xs ->
          let name = Ident.Name.to_string name in
          let global = Llvm.define_global name null m in
          set_linkage global linkage;
          top (`Val (global, t) :: init_list) xs
      | UntypedTree.ValueBinding (name, name', binding, linkage) :: xs ->
          let v = Llvm.bind c ~name:name' ~arity:0 binding m in
          let name = Ident.Name.to_string name in
          let v = Llvm.define_global name (Llvm.const_bitcast v Type.star) m in
          set_linkage v linkage;
          Llvm.set_global_constant true v;
          top init_list xs
      | UntypedTree.FunctionBinding (name, arity, binding) :: xs ->
          let v = Llvm.bind c ~name ~arity binding m in
          top (`Binding (name, v) :: init_list) xs
      | UntypedTree.Exception name :: xs ->
          let name = Ident.Exn.to_string name in
          let v = Llvm.define_global name (string name) m in
          Llvm.set_global_constant true v;
          top init_list xs
      | UntypedTree.Function (name, (name', t), linkage) :: xs ->
          let (f, builder) = Llvm.define_function c (".." ^ Ident.Name.to_string name) (Type.lambda ~env_size:0) m in
          Llvm.set_linkage Llvm.Linkage.Private f;
          define_global ~name ~linkage (Llvm.const_array Type.star [|Llvm.const_bitcast f Type.star|]);
          let g bindings = abs ~f ~name:name' t bindings builder in
          top (`Const g :: init_list) xs
      | [] ->
          let (f, builder) =
            Llvm.define_function c (Generic.init_name I.name) Type.init m
          in
          let jmp_buf = Llvm.param f 0 in
          init_imports ~jmp_buf imports builder;
          let builder = init f builder (List.rev init_list) in
          Llvm.build_ret_void builder;
          m
    in
    top []
end

let make ~modul ~imports options x =
  let module Module =
    Make(struct
      let name = modul
      let debug = options#debug
    end)
  in
  Module.make ~imports x

let main main_module =
  let module Module = Main(struct let main_module = main_module end) in
  Module.make ()

let link ~main_module_name ~main_module imports =
  let aux _ x dst =
    Llvm_linker.link_modules dst x Llvm_linker.Mode.DestroySource;
    dst
  in
  let dst = main_module in
  let src = main main_module_name in
  Llvm_linker.link_modules dst src Llvm_linker.Mode.DestroySource;
  Module.Map.fold aux imports dst

let init = lazy (Llvm_all_backends.initialize ())

let get_triple () =
  Lazy.force init;
  Llvm_target.Target.default_triple ()

let get_target ~triple =
  let target = Llvm_target.Target.by_triple triple in
  Llvm_target.TargetMachine.create ~triple target

let optimize options m =
  let lto = options#lto in
  let opt = options#opt in
  let triple = get_triple () in
  let target = get_target ~triple in
  let layout = Llvm_target.TargetMachine.data_layout target in
  Llvm.set_target_triple triple m;
  Llvm.set_data_layout (Llvm_target.DataLayout.as_string layout) m;
  Llvm.optimize ~lto ~opt layout m;
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
