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

type input_file = string
type output_file = string option

type printer =
  | NoPrinter
  | ParseTree
  | LLVM

type modul =
  { file : ModulePath.t
  ; modul : Gamma.Type.t
  }

exception ParseError of string
exception Break

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

let parse filename parser =
  let aux file =
    let filebuf = Lexing.from_channel file in
    let get_offset () =
      let pos = Lexing.lexeme_start_p filebuf in
      let open Lexing in
      let column = pos.pos_cnum - pos.pos_bol in
      string_of_int pos.pos_lnum ^ ":" ^ string_of_int column
    in
    try parser Lexer.main filebuf with
    | Lexer.Error ->
        raise
          (ParseError (fmt "%s: Lexing error at: %s" filename (get_offset ())))
    | Parser.Error ->
        raise
          (ParseError (fmt "%s: Parsing error at: %s" filename (get_offset ())))
  in
  File.with_file_in filename aux

let rec build_intf path =
  let aux acc module_name =
    let ifile = ModulePath.intf path module_name in
    let ifile = ModulePath.to_string ifile in
    let (imports, tree) = parse ifile Parser.mainInterface in
    let gamma = build_intf path imports in
    let gamma = Interface.compile gamma tree in
    Gamma.union (module_name, gamma) acc
  in
  List.fold_left aux Gamma.empty

let rec build_impl =
  let tbl = Hashtbl.create 32 in
  fun self imports ->
  (* TODO: file that checks the hash of the dependencies *)
  (* TODO: We don't nececary need .sfw in the .sfwi contains only types *)
  let aux ((imports_acc, gamma_acc, impl_acc) as acc) modul =
    match Hashtbl.find tbl modul with
    | Some () ->
        acc
    | None ->
        let interface = build_intf self.file imports in
        let file = ModulePath.impl self.file modul in
        let impl =
          compile
            ~interface
            {file; modul}
        in
        let impl = match impl_acc with
          | Some impl_acc -> Backend.link impl impl_acc
          | None -> impl
        in
        Hashtbl.add tbl modul ();
        let gamma = Gamma.union (modul, interface) gamma_acc in
        (imports_acc @ [modul], gamma, Some impl)
  in
  List.fold_left aux ([], Gamma.empty, None) imports

and compile
      ?(printer = NoPrinter)
      ?(with_main = false)
      ~interface
      self =
  (* TODO: Check interface *)
  let file = ModulePath.to_string self.file in
  let (imports, parse_tree) = parse file Parser.main in
  let (imports, gamma, code) = build_impl self imports in
  let typed_tree = lazy (TypeChecker.from_parse_tree (gamma, parse_tree)) in
  let untyped_tree = lazy (Lambda.of_typed_tree (Lazy.force typed_tree)) in
  let dst = lazy (Backend.make ~with_main ~name:self.modul ~imports (Lazy.force untyped_tree)) in
  let res =
    lazy begin
      let dst = Lazy.force dst in
      match code with
      | Some code -> Backend.link dst code
      | None -> dst
    end
  in
  match printer with
  | ParseTree ->
      (* TODO *)
      raise Break
  | LLVM ->
      print_endline (Backend.to_string (Lazy.force res));
      raise Break
  | NoPrinter ->
      Lazy.force res

let compile ~printer ~lto ~opt ~o file =
  let file = ModulePath.of_file file in
  let modul = ModulePath.to_module file in
  let self = {file; modul} in
  try
    compile ~printer ~with_main:true ~interface:Gamma.empty self
    |> Backend.optimize ~lto ~opt
    |> write ~o
  with
  | Break -> ()
