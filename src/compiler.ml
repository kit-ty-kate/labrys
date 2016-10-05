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

type module_name = string

let fmt = Printf.sprintf

let print_error () =
  prerr_endline "\nThe compilation processes exited abnormally"

let link ~tmp ~o =
  let ld = Utils.exec_command "clang" ["-lgc"; tmp; "-o"; o] in
  if Int.(ld <> 0) then begin
    print_error ();
  end

let with_tmp_file f =
  let tmp = Filename.temp_file "cervoise" "" in
  f tmp;
  Sys.remove tmp

let write ~o result =
  let aux tmp =
    Backend.emit_object_file ~tmp result;
    link ~tmp ~o;
  in
  with_tmp_file aux

let rec build_intf options current_module =
  let module P = ParserHandler.Make(struct let get = Module.intf current_module end) in
  let (mimports, tree) = P.parse_intf () in
  let (mimports, tree) = Builtins.interface ~current_module options mimports tree in
  let mimports = Unsugar.create_imports ~current_module options mimports in
  let (imports, gamma) = build_imports_intf options mimports in
  let (imports, tree) =
    Unsugar.create_interface ~current_module options mimports imports tree
  in
  (imports, Interface.compile ~current_module options gamma tree)

and build_imports_intf options imports =
  let aux (imports, gamma) x =
    let (imports', gamma') = build_intf options x in
    (Imports.union imports imports', Gamma.union gamma' gamma)
  in
  List.fold_left aux (Imports.empty, Gamma.empty) imports

let get_parse_tree modul =
  let module P = ParserHandler.Make(struct let get = Module.impl modul end) in
  P.parse_impl ()

let get_unsugared_tree options modul =
  let (mimports, parse_tree) = get_parse_tree modul in
  let (mimports, parse_tree) =
    Builtins.tree ~current_module:modul options mimports parse_tree
  in
  let mimports = Unsugar.create_imports ~current_module:modul options mimports in
  let (imports, gamma) = build_imports_intf options mimports in
  let unsugared_tree =
    Unsugar.create ~current_module:modul options mimports imports parse_tree
  in
  (mimports, gamma, unsugared_tree)

let get_untyped_tree ~with_main ~interface options modul =
  let (imports, gamma, unsugared_tree) = get_unsugared_tree options modul in
  let typed_tree =
    TypeChecker.check ~modul ~interface ~with_main options gamma unsugared_tree
  in
  (imports, typed_tree)

let get_lambda_tree ~with_main ~interface options modul =
  let (imports, typed_tree) =
    get_untyped_tree ~with_main ~interface options modul
  in
  let lambda_tree = Lambda.of_typed_tree typed_tree in
  (imports, lambda_tree)

let get_reduced_tree ~with_main ~interface options modul =
  let (imports, lambda_tree) =
    get_lambda_tree ~with_main ~interface options modul
  in
  let reduced_tree = Beta.reduce lambda_tree in
  (imports, reduced_tree)

let get_optimized_tree ~with_main ~interface options modul =
  let (imports, reduced_tree) =
    get_reduced_tree ~with_main ~interface options modul
  in
  let optimized_tree = Optimize.of_lambda_tree reduced_tree in
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
      let (imports, reduced_tree) =
        get_optimized_tree ~with_main ~interface options modul
      in
      let imports_code = build_imports ~imports_code options imports in
      let code = Backend.make ~modul ~imports options reduced_tree in
      Backend.write_bitcode ~o:cimpl code;
      BuildSystem.write_impl_infos imports modul;
      (imports_code, code)

let get_code options modul =
  let (imports_code, code) =
    compile ~with_main:true Module.Map.empty Gamma.empty options modul
  in
  prerr_endline (fmt "Linking %s" (Module.to_string modul));
  Backend.link ~main_module_name:modul ~main_module:code imports_code

let get_optimized_code options modul =
  Backend.optimize options (get_code options modul)

let compile_program options modul =
  let modul = Module.from_string options modul in
  let code = get_optimized_code options modul in
  write ~o:options#o code

let compile_module options modul =
  let modul = Module.from_string options modul in
  let (_, _) =
    compile Module.Map.empty Gamma.empty options modul
  in
  prerr_endline (fmt "Module %s compiled" (Module.to_string modul))

let print_parse_tree options modul =
  let modul = Module.from_string options modul in
  let (_, parse_tree) = get_parse_tree modul in
  print_endline (Printers.ParseTree.dump parse_tree)

let print_unsugared_tree options modul =
  let modul = Module.from_string options modul in
  let (_, _, unsugared_tree) = get_unsugared_tree options modul in
  print_endline (Printers.UnsugaredTree.dump unsugared_tree)

let print_untyped_tree options modul =
  let modul = Module.from_string options modul in
  let (_, typed_tree) =
    get_untyped_tree ~with_main:true ~interface:Gamma.empty options modul
  in
  print_endline (Printers.UntypedTree.dump typed_tree)

let print_lambda_tree options modul =
  let modul = Module.from_string options modul in
  let (_, untyped_tree) =
    get_lambda_tree ~with_main:true ~interface:Gamma.empty options modul
  in
  print_endline (Printers.LambdaTree.dump untyped_tree)

let print_reduced_tree options modul =
  let modul = Module.from_string options modul in
  let (_, reduced_tree) =
    get_reduced_tree ~with_main:true ~interface:Gamma.empty options modul
  in
  print_endline (Printers.LambdaTree.dump reduced_tree)

let print_optimized_tree options modul =
  let modul = Module.from_string options modul in
  let (_, optimized_tree) =
    get_optimized_tree ~with_main:true ~interface:Gamma.empty options modul
  in
  print_endline (Printers.OptimizedTree.dump optimized_tree)

let print_early_llvm options modul =
  let modul = Module.from_string options modul in
  let code = get_code options modul in
  print_endline (Backend.to_string code)

let print_llvm options modul =
  let modul = Module.from_string options modul in
  let optimized_code = get_optimized_code options modul in
  print_endline (Backend.to_string optimized_code)

(* TODO: Clean this module *)
