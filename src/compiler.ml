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

open BatteriesExceptionless
open Monomorphic.None

type module_name = string

let fmt = Printf.sprintf

let print_error () =
  prerr_endline "\nThe compilation processes exited abnormally"

let link ~tmp ~o =
  let tmp = Filename.quote tmp in
  let o = Filename.quote o in
  let ld = Sys.command (fmt "cc -lgc %s -o %s" tmp o) in
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
  let current_module = Module.open_module current_module in
  let module P = ParserHandler.Make(struct let get = Module.intf current_module end) in
  let (imports, tree) = P.parse_intf () in
  let (imports, tree) =
    Unsugar.create_interface ~current_module options imports tree
  in
  let aux acc x = Gamma.union ~imported:(build_intf options x) acc in
  let gamma = List.fold_left aux Gamma.empty imports in
  Interface.compile gamma tree

let get_parse_tree modul =
  let module P = ParserHandler.Make(struct let get = Module.impl modul end) in
  P.parse_impl ()

let get_unsugared_tree ~no_prelude options modul =
  let (imports, parse_tree) = get_parse_tree modul in
  let (imports, unsugared_tree) =
    Unsugar.create ~no_prelude ~current_module:modul options imports parse_tree
  in
  (imports, unsugared_tree)

let build_imports_intf options imports =
  let aux gamma modul = Gamma.union ~imported:(build_intf options modul) gamma in
  List.fold_left aux Gamma.empty imports

let get_typed_tree ~with_main ~no_prelude ~interface options modul =
  let (imports, unsugared_tree) = get_unsugared_tree ~no_prelude options modul in
  let gamma = build_imports_intf options imports in
  let typed_tree =
    TypeChecker.check ~modul ~interface ~with_main options gamma unsugared_tree
  in
  (imports, typed_tree)

let get_untyped_tree ~with_main ~no_prelude ~interface options modul =
  let (imports, typed_tree) =
    get_typed_tree ~with_main ~no_prelude ~interface options modul
  in
  let untyped_tree = Lambda.of_typed_tree ~current_module:modul typed_tree in
  (imports, untyped_tree)

let rec build_imports ~imports_code options imports =
  let aux imports_code modul =
    if Module.Map.mem modul imports_code then begin
      imports_code
    end else begin
      let interface = build_intf options modul in
      let (imports_code, code) = compile imports_code interface options modul in
      Module.Map.add modul code imports_code
    end
  in
  List.fold_left aux imports_code imports

and compile ?(with_main=false) ?(no_prelude=false) imports_code interface options modul =
  let modul = Module.open_module modul in
  let cimpl = Module.cimpl modul in
  try
    let imports = BuildSystem.check_impl options modul in
    let imports_code = build_imports ~imports_code options imports in
    let code = Backend.read_bitcode cimpl in
    (imports_code, code)
  with
  | BuildSystem.Failure ->
      if Module.is_library modul then
        Error.fail_module
          "The library %s cannot be collected"
          (Module.to_string modul);
      prerr_endline (fmt "Compiling %s" (Module.to_string modul));
      let (imports, untyped_tree) = get_untyped_tree ~with_main ~no_prelude ~interface options modul in
      let imports_code = build_imports ~imports_code options imports in
      let code = Backend.make ~modul ~imports untyped_tree in
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
  let no_prelude = options#no_prelude in
  let (_, _) =
    compile ~no_prelude Module.Map.empty Gamma.empty options modul
  in
  prerr_endline (fmt "Module %s compiled" (Module.to_string modul))

let print_parse_tree options modul =
  let modul = Module.from_string options modul in
  let modul = Module.open_module modul in
  let (_, parse_tree) = get_parse_tree modul in
  print_endline (Printers.ParseTree.dump parse_tree)

let print_unsugared_tree options modul =
  let modul = Module.from_string options modul in
  let modul = Module.open_module modul in
  let no_prelude = options#no_prelude in
  let (_, unsugared_tree) = get_unsugared_tree ~no_prelude options modul in
  print_endline (Printers.UnsugaredTree.dump unsugared_tree)

let print_typed_tree options modul =
  let modul = Module.from_string options modul in
  let modul = Module.open_module modul in
  let no_prelude = options#no_prelude in
  let (_, typed_tree) =
    get_typed_tree
      ~with_main:true
      ~no_prelude
      ~interface:Gamma.empty
      options
      modul
  in
  print_endline (Printers.TypedTree.dump typed_tree)

let print_untyped_tree options modul =
  let modul = Module.from_string options modul in
  let modul = Module.open_module modul in
  let no_prelude = options#no_prelude in
  let (_, untyped_tree) =
    get_untyped_tree
      ~with_main:true
      ~no_prelude
      ~interface:Gamma.empty
      options
      modul
  in
  print_endline (Printers.UntypedTree.dump untyped_tree)

let print_early_llvm options modul =
  let modul = Module.from_string options modul in
  let code = get_code options modul in
  print_endline (Backend.to_string code)

let print_llvm options modul =
  let modul = Module.from_string options modul in
  let optimized_code = get_optimized_code options modul in
  print_endline (Backend.to_string optimized_code)
