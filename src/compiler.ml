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

type input_file = string
type directory = string
type output_file = string option

type printer =
  | NoPrinter
  | ParseTree
  | UnsugaredTree
  | TypedTree
  | UntypedTree
  | LLVM
  | OptimizedLLVM

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
  let o = Option.default "a.out" o in
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

let rec build_intf parent_module =
  let module P = ParserHandler.Make(struct let get = ModulePath.intf parent_module end) in
  let (imports, tree) = P.parse_intf () in
  let imports = Unsugar.create_imports imports in
  let tree = Unsugar.create_interface tree in
  let aux acc x =
    let x = ModulePath.of_module ~parent_module x in
    Gamma.union (ModulePath.to_module x, build_intf x) acc
  in
  let gamma = List.fold_left aux Gamma.empty imports in
  Interface.compile gamma tree

let rec build_impl =
  let tbl = Hashtbl.create 32 in
  fun ~build_dir parent_module imports ->
  (* TODO: file that checks the hash of the dependencies *)
  (* TODO: We don't nececary need .sfw in the .sfwi contains only types *)
  let aux ((imports_acc, gamma_acc, impl_acc) as acc) modul =
    let modul = ModulePath.of_module ~parent_module modul in
    match Hashtbl.find tbl modul with
    | Some () ->
        acc
    | None ->
        let interface = build_intf modul in
        let (_, _, _, _, impl, imports_impl) = compile ~build_dir ~interface modul in
        Hashtbl.add tbl modul ();
        let gamma = Gamma.union (ModulePath.to_module modul, interface) gamma_acc in
        (imports_acc @ [modul], gamma, impl :: imports_impl @ impl_acc)
  in
  List.fold_left aux ([], Gamma.empty, []) imports

and compile ~build_dir ?(with_main=false) ~interface modul =
  let module P = ParserHandler.Make(struct let get = ModulePath.impl modul end) in
  let (imports, parse_tree) = P.parse_impl () in
  let imports = Unsugar.create_imports imports in
  (* TODO: Print with and without builtins *)
  let unsugared_tree = lazy (Unsugar.create parse_tree) in
  let unsugared_tree =
    lazy (prepend_builtins (Lazy.force unsugared_tree))
  in
  let (imports, gamma, imports_code) = build_impl ~build_dir modul imports in
  let typed_tree =
    lazy begin
      TypeChecker.check ~modul ~interface ~with_main gamma (Lazy.force unsugared_tree)
    end
  in
  let untyped_tree = lazy (Lambda.of_typed_tree (Lazy.force typed_tree)) in
  let name = ModulePath.to_module modul in
  let code =
    lazy begin
      let cimpl = ModulePath.cimpl ~build_dir modul in
      if Sys.file_exists cimpl then begin
        Backend.read_bitcode cimpl
      end else begin
        let untyped_tree = Lazy.force untyped_tree in
        let code = Backend.make ~name ~imports untyped_tree in
        if not (Backend.write_bitcode ~o:cimpl code) then
          Error.fail_module "Module '%s' cannot be written to a file" cimpl;
        code
      end
    end
  in
  prerr_endline (fmt "Compiling %s" (Ident.Module.to_module_name name));
  (parse_tree, unsugared_tree, typed_tree, untyped_tree, code, imports_code)

let compile ~printer ~lto ~opt ~build_dir ~o modul =
  let modul = ModulePath.create modul in
  let (parse_tree, unsugared_tree, typed_tree, untyped_tree, code, imports_code) =
    compile ~build_dir ~with_main:true ~interface:Gamma.empty modul
  in
  let code =
    lazy begin
      Utils.mkdir build_dir 0o750;
      let code = Lazy.force code in
      let imports_code = List.map Lazy.force imports_code in
      let main_module_name = ModulePath.to_module modul in
      Backend.link ~main_module_name ~main_module:code imports_code
    end
  in
  let optimized_res = lazy (Backend.optimize ~lto ~opt (Lazy.force code)) in
  begin match printer with
  | ParseTree ->
      print_endline (Printers.ParseTree.dump parse_tree);
  | UnsugaredTree ->
      print_endline (Printers.UnsugaredTree.dump (Lazy.force unsugared_tree));
  | TypedTree ->
      print_endline (Printers.TypedTree.dump (Lazy.force typed_tree));
  | UntypedTree ->
      print_endline (Printers.UntypedTree.dump (Lazy.force untyped_tree));
  | LLVM ->
      print_endline (Backend.to_string (Lazy.force code));
  | OptimizedLLVM ->
      print_endline (Backend.to_string (Lazy.force optimized_res));
  | NoPrinter ->
      write ~o (Lazy.force optimized_res)
  end
