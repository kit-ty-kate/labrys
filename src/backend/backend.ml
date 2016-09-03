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

open Containers
open Monomorphic.None

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

let exn_glob_size = succ Config.max_fail_num_args

module Type = struct
  let void = Llvm.void_type c
  let i8 = Llvm.i8_type c
  let i32 = Llvm.i32_type c
  let float = Llvm.float_type c
  let star = Llvm.pointer_type i8
  let array = Llvm.array_type star
  let exn_glob = array exn_glob_size
  let array_ptr size = Llvm.pointer_type (array size)
  let variant_ptr = array_ptr
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

  let alloc_jmp_buf builder =
    let jmp_buf = Llvm.build_alloca Type.jmp_buf "" builder in
    let v = Llvm.undef Type.jmp_buf in
    let fp = Llvm.build_call frameaddress [|i32 0|] "" builder in
    let v = Llvm.build_insertvalue v fp 0 "" builder in
    let sp = Llvm.build_call stacksave [||] "" builder in
    let v = Llvm.build_insertvalue v sp 2 "" builder in
    Llvm.build_store v jmp_buf builder;
    jmp_buf
end

module Main (I : sig val main_module : Module.t end) = struct
  let m = Llvm.create_module c "_main_"

  module Generic = Generic (struct let m = m end)

  let () =
    let malloc_type = Llvm.function_type Type.star [|Type.i32|] in
    let (malloc, builder) = Llvm.define_function `External c "malloc" malloc_type m in
    let gc_malloc = Llvm.declare_function "GC_malloc" malloc_type m in
    Llvm.build_ret (Llvm.build_call gc_malloc (Llvm.params malloc) "" builder) builder

  let create_builtin_instruction ty name g =
    let ty = Llvm.function_type ty [|ty; ty|] in
    let (f, builder) = Llvm.define_function `External c (fmt ".cervoise.%s" name) ty m in
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
    let gc_init = Llvm.declare_function "GC_init" Type.unit_function m in
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

  let malloc_and_init size values builder =
    let ty = Type.array size in
    let allocated = Llvm.build_malloc ty "" builder in
    init allocated ty values builder;
    Llvm.build_bitcast allocated Type.star "" builder

  let malloc_and_init values builder =
    match List.length values with
    | 0 -> null
    | size -> malloc_and_init size values builder

  let debug_trap = Llvm.declare_function "llvm.debugtrap" Type.unit_function m

  let load_env builder =
    let env = Llvm.current_param builder 1 in
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

  let exn_var =
    let v = Llvm.define_global "exn" (Llvm.undef Type.exn_glob) m in
    Llvm.set_thread_local true v;
    Llvm.set_linkage Llvm.Linkage.Link_once_odr v;
    v

  let fold_env gamma builder =
    let aux name value (i, values, gamma) =
      match value with
      | Value value ->
          let values = value :: values in
          let gamma = GammaMap.Value.add name (Env i) gamma in
          (succ i, values, gamma)
      | Env j ->
          let env = load_env builder in
          let value = Llvm.build_extractvalue env j "" builder in
          let values = value :: values in
          let gamma = GammaMap.Value.add name (Env i) gamma in
          (succ i, values, gamma)
      | RecFun ->
          let value = Llvm.current_param builder 1 in
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

  let create_closure ~isrec ~used_vars gamma builder =
    let gamma = GammaMap.Value.filter (fun x _ -> Set.mem x used_vars) gamma in
    let (values, gamma) = fold_env gamma builder in
    let env_size = List.length values in
    let gamma = match isrec with
      | Some rec_name when Set.mem rec_name used_vars ->
          GammaMap.Value.add rec_name RecFun gamma
      | Some _ | None ->
          gamma
    in
    let env_size = succ env_size in
    let (f, builder') = Llvm.define_function `Private c "__lambda" (Type.lambda ~env_size) m in
    Llvm.set_linkage Llvm.Linkage.Private f;
    let f = Llvm.build_bitcast f Type.star "" builder in
    let closure = malloc_and_init (f :: values) builder in
    (builder', closure, gamma)

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
              let value = Llvm.build_load_cast value (Type.variant_ptr (succ i)) builder in
              (Llvm.build_extractvalue value i "" builder, vars)
        in
        (value, Map.add var value vars)

  let get_const = function
    | LambdaTree.Int n -> Llvm.const_int Type.i32 n
    | LambdaTree.Float n -> Llvm.const_float Type.float n
    | LambdaTree.Char n -> Llvm.const_int Type.i32 n
    | LambdaTree.String s -> Llvm.const_string c (s ^ "\x00")

  let llvm_ty_of_ty = function
    | LambdaTree.Int () -> Type.i32
    | LambdaTree.Float () -> Type.float
    | LambdaTree.Char () -> Type.i32
    | LambdaTree.String () -> Type.star

  let ret_type = function
    | LambdaTree.Void _ -> Type.void
    | LambdaTree.Alloc ty -> llvm_ty_of_ty ty

  let args_type args =
    let aux (ty, _) = llvm_ty_of_ty ty in
    Array.of_list (List.map aux args)

  let create_fail jmp_buf builder =
    let jmp_buf = Llvm.build_bitcast jmp_buf Type.star "" builder in
    Llvm.build_call_void longjmp [|jmp_buf|] builder;
    unreachable builder;
    (undef, snd (Llvm.create_block c builder))

  let get_value gamma builder name =
    match GammaMap.Value.find_opt name gamma with
    | Some (Global value) | Some (Value value) ->
        value
    | Some (Env i) ->
        let env = load_env builder in
        Llvm.build_extractvalue env i "" builder
    | Some RecFun ->
        let value = Llvm.current_param builder 1 in
        let value = Llvm.build_bitcast value Type.star "" builder in
        value
    | None ->
        let name = Ident.Name.to_string name in
        let extern = Llvm.declare_global Type.star name m in
        Llvm.build_load extern "" builder

  let map_args gamma builder = function
    | [] ->
        [||]
    | args ->
        let aux = function
          | (LambdaTree.String (), name) ->
              get_value gamma builder name
          | (ty, name) ->
              let ty = Llvm.pointer_type (llvm_ty_of_ty ty) in
              let v = get_value gamma builder name in
              Llvm.build_load_cast v ty builder
        in
        Array.of_list (List.map aux args)

  let rec build_if_chain ~default vars gamma builder value results f term cases =
    let aux builder (constr, tree) =
      let if_branch =
        let (block, builder) = Llvm.create_block c builder in
        create_tree' ~default vars gamma builder value results f tree;
        block
      in
      let (else_branch, else_builder) = Llvm.create_block c builder in
      let constr = f constr in
      let cmp = Llvm.build_icmp Llvm.Icmp.Eq term constr "" builder in
      Llvm.build_cond_br cmp if_branch else_branch builder;
      else_builder
    in
    let builder = List.fold_left aux builder cases in
    Llvm.build_br default builder

  and create_tree' ~default vars gamma builder value results f = function
    | LambdaTree.Leaf i ->
        let (block, _, pattern_vars) = List.nth results i in
        let aux vars (var, variable) =
          let (var, vars) = llvalue_of_pattern_var vars value builder var in
          Llvm.build_store var variable builder;
          vars
        in
        ignore (List.fold_left aux vars pattern_vars);
        Llvm.build_br block builder
    | LambdaTree.Node (var, cases) ->
        let (term, vars) = llvalue_of_pattern_var vars value builder var in
        let term = Llvm.build_load_cast term (Type.variant_ptr 1) builder in
        let term = Llvm.build_extractvalue term 0 "" builder in
        build_if_chain ~default vars gamma builder value results f term cases

  let create_tree ~default vars gamma builder value results =
    let g f tree = create_tree' ~default vars gamma builder value results f tree in
    let int_to_ptr i = Llvm.const_inttoptr (Llvm.const_int Type.i32 i) Type.star in
    function
    | LambdaTree.IdxTree tree -> g int_to_ptr tree
    | LambdaTree.PtrTree tree -> g get_exn tree

  let rec map_ret ~jmp_buf gamma builder t = function
    | LambdaTree.Void t ->
        lambda ~jmp_buf gamma builder t
    | LambdaTree.Alloc (LambdaTree.String ()) ->
        (t, builder)
    | LambdaTree.Alloc ty ->
        let ty = llvm_ty_of_ty ty in
        let value = Llvm.build_malloc ty "" builder in
        Llvm.build_store t value builder;
        let value = Llvm.build_bitcast value Type.star "" builder in
        (value, builder)

  and create_result ~jmp_buf ~next_block gamma builder (vars, result) =
    let (block, builder') = Llvm.create_block c builder in
    let (gamma, pattern_vars) =
      let aux (gamma, pattern_vars) (var, name) =
        let variable = Llvm.build_alloca Type.star "" builder in
        let value = Llvm.build_load variable "" builder' in
        (GammaMap.Value.add name (Value value) gamma, (var, variable) :: pattern_vars)
      in
      List.fold_left aux (gamma, []) vars
    in
    let (v, builder'') = lambda ~jmp_buf gamma builder' result in
    Llvm.build_br next_block builder'';
    (block, (v, Llvm.insertion_block builder''), pattern_vars)

  and abs ~name t gamma builder =
    let param = Llvm.current_param builder 0 in
    let jmp_buf = Llvm.current_param builder 2 in
    let gamma = GammaMap.Value.add name (Value param) gamma in
    let (v, builder) = lambda ~jmp_buf gamma builder t in
    Llvm.build_ret v builder

  and lambda ?isrec ~jmp_buf gamma builder = function
    | LambdaTree.Abs (name, used_vars, t) ->
        let (builder', closure, gamma) =
          create_closure ~isrec ~used_vars gamma builder
        in
        abs ~name t gamma builder';
        (closure, builder)
    | LambdaTree.App (f, x) ->
        let (closure, builder) = lambda ~jmp_buf gamma builder f in
        let x = get_value gamma builder x in
        let closure = Llvm.build_bitcast closure (Type.closure_ptr 1) "" builder in
        let f = Llvm.build_load closure "" builder in
        let f = Llvm.build_extractvalue f 0 "" builder in
        let f = Llvm.build_bitcast f (Type.lambda_ptr ~env_size:1) "" builder in
        (Llvm.build_call f [|x; closure; jmp_buf|] "" builder, builder)
    | LambdaTree.PatternMatching (t, results, default, tree) ->
        let (t, builder) = lambda ~jmp_buf gamma builder t in
        let (next_block, next_builder) = Llvm.create_block c builder in
        let results = List.map (create_result ~next_block ~jmp_buf gamma builder) results in
        let default =
          let (block, builder) = Llvm.create_block c builder in
          let (_, builder') = lambda ~jmp_buf gamma builder default in
          Llvm.build_unreachable builder';
          block
        in
        create_tree ~default Map.empty gamma builder t results tree;
        let results = List.map (fun (_, x, _) -> x) results in
        (Llvm.build_phi results "" next_builder, next_builder)
    | LambdaTree.Val name ->
        (get_value gamma builder name, builder)
    | LambdaTree.Datatype (index, params) ->
        let aux (acc, builder) t =
          let (t, builder) = lambda ~jmp_buf gamma builder t in
          (t :: acc, builder)
        in
        let (values, builder) = List.fold_left aux ([], builder) params in
        let values = List.rev values in
        let values = match index with
          | Some index -> Llvm.const_inttoptr (i32 index) Type.star :: values
          | None -> values
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
    | LambdaTree.CallForeign (name, ret, args) ->
        let ty = Llvm.function_type (ret_type ret) (args_type args) in
        let f = Llvm.declare_function name ty m in
        let args = map_args gamma builder args in
        map_ret ~jmp_buf gamma builder (Llvm.build_call f args "" builder) ret
    | LambdaTree.Let (name, LambdaTree.NonRec, t, xs) ->
        let (t, builder) = lambda ~jmp_buf gamma builder t in
        let gamma = GammaMap.Value.add name (Value t) gamma in
        lambda ~jmp_buf gamma builder xs
    | LambdaTree.Let (name, LambdaTree.Rec, t, xs) ->
        let (t, builder) = lambda ~isrec:name ~jmp_buf gamma builder t in
        let gamma = GammaMap.Value.add name (Value t) gamma in
        lambda ~jmp_buf gamma builder xs
    | LambdaTree.Fail (name, args) ->
        let (args, builder) = fold_args ~jmp_buf gamma builder args in
        let tag = get_exn name in
        init exn_var Type.exn_glob (tag :: args) builder;
        create_fail jmp_buf builder
    | LambdaTree.Try (t, (name, t')) ->
        let jmp_buf' = Generic.alloc_jmp_buf builder in
        let (next_block, next_builder) = Llvm.create_block c builder in
        let jmp_buf_gen = Llvm.build_bitcast jmp_buf' Type.star "" builder in
        let jmp_res = Llvm.build_call setjmp [|jmp_buf_gen|] "" builder in
        let cond = Llvm.build_icmp Llvm.Icmp.Eq jmp_res (i32 0) "" builder in
        let (try_result, try_block) =
          let (block, builder) = Llvm.create_block c builder in
          let (t, builder') = lambda ~jmp_buf:jmp_buf' gamma builder t in
          Llvm.build_br next_block builder';
          ((t, Llvm.insertion_block builder'), block)
        in
        let (catch_result, catch_block) =
          let (block, builder) = Llvm.create_block c builder in
          let exn = Llvm.build_alloca Type.exn_glob "" builder in
          let exn' = Llvm.build_load exn_var "" builder in
          Llvm.build_store exn' exn builder;
          Llvm.build_store (Llvm.undef Type.exn_glob) exn_var builder;
          let gamma = GammaMap.Value.add name (Value exn) gamma in
          let (t', builder') = lambda ~jmp_buf gamma builder t' in
          Llvm.build_br next_block builder';
          ((t', Llvm.insertion_block builder'), block)
        in
        Llvm.build_cond_br cond try_block catch_block builder;
        (Llvm.build_phi [try_result; catch_result] "" next_builder, next_builder)
    | LambdaTree.RecordGet (t, n) ->
        let (t, builder) = lambda ~jmp_buf gamma builder t in
        let t = Llvm.build_load_cast t (Type.array_ptr (succ n)) builder in
        (Llvm.build_extractvalue t n "" builder, builder)
    | LambdaTree.Const const ->
        let v = Llvm.define_constant "" (get_const const) m in
        let v = Llvm.const_bitcast v Type.star in
        (v, builder)
    | LambdaTree.Unreachable ->
        unreachable builder;
        (undef, snd (Llvm.create_block c builder))
    | LambdaTree.Reraise e ->
        let e = get_value gamma builder e in
        let e = Llvm.build_load e "" builder in
        Llvm.build_store e exn_var builder;
        create_fail jmp_buf builder

  and fold_args ~jmp_buf gamma builder args =
    let aux (acc, builder) x =
      let (x, builder) = lambda ~jmp_buf gamma builder x in
      (x :: acc, builder)
    in
    let (args, builder) = List.fold_left aux ([], builder) args in
    (List.rev args, builder)

  let set_linkage v = function
    | LambdaTree.Private -> Llvm.set_linkage Llvm.Linkage.Private v
    | LambdaTree.Public -> Llvm.set_linkage Llvm.Linkage.External v

  let define_global ~name ~linkage value =
    let name = Ident.Name.to_string name in
    let name' = "." ^ name in
    let v = Llvm.define_constant name' value m in
    let v = Llvm.define_constant name (Llvm.const_bitcast v Type.star) m in
    set_linkage v linkage

  let rec init ~jmp_buf bindings builder = function
    | `Val (global, t) :: xs ->
        let (value, builder) = lambda ~jmp_buf bindings builder t in
        Llvm.build_store value global builder;
        init ~jmp_buf bindings builder xs
    | `Const g :: xs ->
        g bindings;
        init ~jmp_buf bindings builder xs
    | `Binding (name, v) :: xs ->
        let bindings = GammaMap.Value.add name (Global v) bindings in
        init ~jmp_buf bindings builder xs
    | [] ->
        builder

  let init_imports ~jmp_buf imports builder =
    let aux import =
      let f = Llvm.declare_global Type.init (Generic.init_name import) m in
      Llvm.build_call_void f [|jmp_buf|] builder
    in
    List.iter aux imports

  let make ~imports =
    let rec top init_list = function
      | LambdaTree.Value (name, t, linkage) :: xs ->
          let name = Ident.Name.to_string name in
          let global = Llvm.define_global name null m in
          set_linkage global linkage;
          top (`Val (global, t) :: init_list) xs
      | LambdaTree.Exception name :: xs ->
          let name = Ident.Exn.to_string name in
          (* NOTE: Don't use Llvm.define_constant as it merges equal values *)
          let v = Llvm.define_global name (string name) m in
          Llvm.set_global_constant true v;
          top init_list xs
      | LambdaTree.Function (name, (name', t), linkage) :: xs ->
          let (f, builder) = Llvm.define_function `Private c (".." ^ Ident.Name.to_string name) (Type.lambda ~env_size:0) m in
          define_global ~name ~linkage (Llvm.const_array Type.star [|Llvm.const_bitcast f Type.star|]);
          let g bindings = abs ~name:name' t bindings builder in
          top (`Const g :: init_list) xs
      | [] ->
          let (f, builder) =
            Llvm.define_function `External c (Generic.init_name I.name) Type.init m
          in
          let jmp_buf = Llvm.param f 0 in
          init_imports ~jmp_buf imports builder;
          let builder = init ~jmp_buf GammaMap.Value.empty builder (List.rev init_list) in
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
    Llvm_linker.link_modules' dst x;
    dst
  in
  let dst = main_module in
  let () =
    let src = main main_module_name in
    Llvm_linker.link_modules' dst src;
  in
  Module.Map.fold aux imports dst

let init = lazy (Llvm_all_backends.initialize ())

let get_triple () =
  Lazy.force init;
  Llvm_target.Target.default_triple ()

let get_target ~triple =
  let target = Llvm_target.Target.by_triple triple in
  Llvm_target.TargetMachine.create ~triple target

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
