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

(* TODO: Do better *)
let prepend_builtins tree =
  let loc = Builtins.unknown_loc in
  let ty = (loc, UnsugaredTree.Ty Builtins.t_unit) in
  let variants = [UnsugaredTree.Variant (Builtins.unit, ty)] in
  UnsugaredTree.Datatype (Builtins.t_unit, Kinds.Star, variants) :: tree

let rec build_intf current_module =
  let module P = ParserHandler.Make(struct let get = Module.intf current_module end) in
  let (imports, tree) = P.parse_intf () in
  let imports = Unsugar.create_imports ~current_module imports in
  let tree = Unsugar.create_interface tree in
  let aux acc x =
    Gamma.union (x, build_intf x) acc
  in
  let gamma = List.fold_left aux Gamma.empty imports in
  Interface.compile gamma tree

let rec build_impl =
  let tbl = Hashtbl.create 32 in
  fun options imports ->
  (* TODO: file that checks the hash of the dependencies *)
  (* TODO: We don't nececary need .sfw in the .sfwi contains only types *)
  let aux ((imports_acc, gamma_acc, impl_acc) as acc) modul =
    match Hashtbl.find tbl modul with
    | Some () ->
        acc
    | None ->
        let interface = build_intf modul in
        let (_, _, _, _, impl, imports_impl) = compile ~interface options modul in
        Hashtbl.add tbl modul ();
        let gamma = Gamma.union (modul, interface) gamma_acc in
        (imports_acc @ [modul], gamma, impl :: imports_impl @ impl_acc)
  in
  List.fold_left aux ([], Gamma.empty, []) imports

and compile ?(with_main=false) ~interface options modul =
  let module P = ParserHandler.Make(struct let get = Module.impl modul end) in
  let (imports, parse_tree) = P.parse_impl () in
  let imports = Unsugar.create_imports ~current_module:modul imports in
  (* TODO: Print with and without builtins *)
  let unsugared_tree = Unsugar.create parse_tree in
  let unsugared_tree = prepend_builtins unsugared_tree in
  let (imports, gamma, imports_code) = build_impl options imports in
  let typed_tree =
    TypeChecker.check ~modul ~interface ~with_main gamma unsugared_tree
  in
  let untyped_tree = Lambda.of_typed_tree typed_tree in
  let code =
    let cimpl = Module.cimpl modul in
    try
      BuildSystem.check_impl options modul;
      Backend.read_bitcode cimpl
    with
    | BuildSystem.Failure ->
        let code = Backend.make ~modul ~imports untyped_tree in
        if not (Backend.write_bitcode ~o:cimpl code) then
          Error.fail_module "Module '%s' cannot be written to a file" cimpl;
        BuildSystem.write_impl_infos imports modul;
        code
  in
  prerr_endline (fmt "Compiling %s" (Module.to_string modul));
  (parse_tree, unsugared_tree, typed_tree, untyped_tree, code, imports_code)

let compile options modul =
  let modul = Module.from_string options modul in
  let (parse_tree, unsugared_tree, typed_tree, untyped_tree, code, imports_code) =
    compile ~with_main:true ~interface:Gamma.empty options modul
  in
  let code =
    Backend.link ~main_module_name:modul ~main_module:code imports_code
  in
  let optimized_res = Backend.optimize options code in
  begin match options.Options.printer with
  | Options.ParseTree ->
      print_endline (Printers.ParseTree.dump parse_tree);
  | Options.UnsugaredTree ->
      print_endline (Printers.UnsugaredTree.dump unsugared_tree);
  | Options.TypedTree ->
      print_endline (Printers.TypedTree.dump typed_tree);
  | Options.UntypedTree ->
      print_endline (Printers.UntypedTree.dump untyped_tree);
  | Options.LLVM ->
      print_endline (Backend.to_string code);
  | Options.OptimizedLLVM ->
      print_endline (Backend.to_string optimized_res);
  | Options.NoPrinter ->
      write ~o:options.Options.o optimized_res
  end
