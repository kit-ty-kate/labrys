(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type module_name = string

let fmt = Printf.sprintf

let print_error () =
  prerr_endline "\nThe compilation processes exited abnormally"

let link ~cc ~tmp ~o =
  let ld = Utils.exec_command cc [tmp; "-lgc"; "-o"; o] in
  if Int.(ld <> 0) then begin
    print_error ();
  end

let with_tmp_file f =
  let tmp = Filename.temp_file "cervoise" "" in
  f tmp;
  Sys.remove tmp

let write ~cc ~o result =
  let aux tmp =
    Backend.emit_object_file ~tmp result;
    link ~cc ~tmp ~o;
  in
  with_tmp_file aux

let rec build_intf options current_module =
  let module P = ParserHandler.Make(struct let get = Module.intf current_module end) in
  let (mimports, tree) = P.parse_intf () in
  let (mimports, tree) = Builtins.interface ~current_module options mimports tree in
  let mimports = Desugar.create_imports ~current_module options mimports in
  let (imports, env) = build_imports_intf options mimports in
  let (imports, tree) =
    Desugar.create_interface ~current_module options mimports imports tree
  in
  let tree = Pretyper.pretype_interface tree in
  (imports, TypeChecker.check_interface env tree)

and build_imports_intf options imports =
  let aux (imports, env) x =
    let (imports', env') = build_intf options x in
    (Imports.union imports imports', Env.union env' env)
  in
  List.fold_left aux (Imports.empty, Env.empty) imports

let get_parse_tree modul =
  let module P = ParserHandler.Make(struct let get = Module.impl modul end) in
  P.parse_impl ()

let get_desugared_tree options modul =
  let (mimports, parse_tree) = get_parse_tree modul in
  let (mimports, parse_tree) =
    Builtins.tree ~current_module:modul options mimports parse_tree
  in
  let mimports = Desugar.create_imports ~current_module:modul options mimports in
  let (imports, env) = build_imports_intf options mimports in
  let desugared_tree =
    Desugar.create ~current_module:modul options mimports imports parse_tree
  in
  (mimports, env, desugared_tree)

let get_pretyped_tree options modul =
  let (imports, env, desugared_tree) = get_desugared_tree options modul in
  let pretyped_tree = Pretyper.pretype desugared_tree in
  (imports, env, pretyped_tree)

let get_untyped_tree ~with_main ~interface options modul =
  let (imports, env, pretyped_tree) = get_pretyped_tree options modul in
  let options = object
    method lib_dir = options#lib_dir
    method with_main = with_main
  end in
  let typed_tree =
    TypeChecker.check ~current_module:modul ~interface options env pretyped_tree
  in
  (imports, env, typed_tree)

let get_lambda_tree ~with_main ~interface options modul =
  let (imports, env, typed_tree) =
    get_untyped_tree ~with_main ~interface options modul
  in
  let lambda_tree = Lambda.of_typed_tree env typed_tree in
  (imports, lambda_tree)

let get_flatten_tree ~with_main ~interface options modul =
  let (imports, lambda_tree) =
    get_lambda_tree ~with_main ~interface options modul
  in
  let flatten_tree = Flatten.of_lambda_tree lambda_tree in
  (imports, flatten_tree)

let get_optimized_tree ~with_main ~interface options modul =
  let (imports, flatten_tree) =
    get_flatten_tree ~with_main ~interface options modul
  in
  let optimized_tree = Optimize.of_flatten_tree flatten_tree in
  (imports, optimized_tree)

let rec build_imports ~imports_code options imports =
  let aux imports_code modul =
    if Module.Map.mem modul imports_code then begin
      imports_code
    end else begin
      let (_, interface) = build_intf options modul in
      let (imports_code, code) =
        compile imports_code interface options modul
      in
      Module.Map.add modul code imports_code
    end
  in
  List.fold_left aux imports_code imports

and compile ?(with_main=false) imports_code interface options modul =
  let cimpl = Module.cimpl modul in
  try
    let imports = BuildSystem.check_impl options modul in
    let imports_code = build_imports ~imports_code options imports in
    let code = Backend.read_bitcode cimpl in
    (imports_code, code)
  with
  | BuildSystem.Failure ->
      if Module.is_library modul then
        Err.fail_module
          "The library %s cannot be collected"
          (Module.to_string modul);
      prerr_endline (fmt "Compiling %s" (Module.to_string modul));
      let (imports, lambda_tree) =
        get_optimized_tree ~with_main ~interface options modul
      in
      let imports_code = build_imports ~imports_code options imports in
      let code = Backend.make ~modul ~imports options lambda_tree in
      Backend.write_bitcode ~o:cimpl code;
      BuildSystem.write_impl_infos imports modul;
      (imports_code, code)

let get_code options modul =
  let (imports_code, code) =
    compile ~with_main:true Module.Map.empty Env.empty options modul
  in
  prerr_endline (fmt "Linking %s" (Module.to_string modul));
  Backend.link ~main_module_name:modul ~main_module:code imports_code

let get_optimized_code options modul =
  Backend.optimize options (get_code options modul)

let compile_program modul options =
  let modul = Module.from_string options modul in
  let code = get_optimized_code options modul in
  write ~cc:options#cc ~o:options#o code

let compile_module modul options =
  let modul = Module.from_string options modul in
  let (_, _) =
    compile Module.Map.empty Env.empty options modul
  in
  prerr_endline (fmt "Module %s compiled" (Module.to_string modul))

let print_parse_tree modul options =
  let modul = Module.from_string options modul in
  let (_, parse_tree) = get_parse_tree modul in
  print_endline (Utils.string_of_doc (ParseTreePrinter.dump parse_tree))

let print_desugared_tree modul options =
  let modul = Module.from_string options modul in
  let (_, _, desugared_tree) = get_desugared_tree options modul in
  print_endline (Utils.string_of_doc (DesugaredTreePrinter.dump desugared_tree))

let print_pretyped_tree modul options =
  let modul = Module.from_string options modul in
  let (_, _, pretyped_tree) = get_pretyped_tree options modul in
  print_endline (Utils.string_of_doc (PretypedTreePrinter.dump pretyped_tree))

let print_untyped_tree modul options =
  let modul = Module.from_string options modul in
  let (_, _, typed_tree) =
    get_untyped_tree ~with_main:true ~interface:Env.empty options modul
  in
  print_endline (Printers.UntypedTree.dump typed_tree)

let print_lambda_tree modul options =
  let modul = Module.from_string options modul in
  let (_, lambda_tree) =
    get_lambda_tree ~with_main:true ~interface:Env.empty options modul
  in
  print_endline (Printers.LambdaTree.dump lambda_tree)

let print_flatten_tree modul options =
  let modul = Module.from_string options modul in
  let (_, flatten_tree) =
    get_flatten_tree ~with_main:true ~interface:Env.empty options modul
  in
  print_endline (Printers.FlattenTree.dump flatten_tree)

let print_optimized_tree modul options =
  let modul = Module.from_string options modul in
  let (_, optimized_tree) =
    get_optimized_tree ~with_main:true ~interface:Env.empty options modul
  in
  print_endline (Printers.OptimizedTree.dump optimized_tree)

let print_early_llvm modul options =
  let modul = Module.from_string options modul in
  let code = get_code options modul in
  print_endline (Backend.to_string code)

let print_llvm modul options =
  let modul = Module.from_string options modul in
  let optimized_code = get_optimized_code options modul in
  print_endline (Backend.to_string optimized_code)

(* TODO: Clean this module *)
