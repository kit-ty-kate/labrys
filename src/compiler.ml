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

type args =
  { print : bool
  ; lto : bool
  ; opt : int
  ; o : string option
  ; file : string
  }

exception ParseError of string

let fmt = Printf.sprintf

let impl_of_module modul =
  let base_filename = Gamma.Type.to_file modul in
  base_filename ^ ".sfw"

let intf_of_module modul = impl_of_module modul ^ "i"

let module_of_file file =
  let file = Filename.chop_extension file in
  let file = String.nsplit file ~by:"/" in
  let file = List.map String.capitalize file in
  String.concat "_" file

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

(* TODO: handle steps in path *)
let rec build_intf =
  let aux acc module_name =
    let ifile = intf_of_module module_name in
    let (imports, tree) = parse ifile Parser.mainInterface in
    let gamma = build_intf imports in
    let gamma = Interface.compile gamma tree in
    Gamma.union (module_name, gamma) acc
  in
  fun imports -> List.fold_left aux Gamma.empty imports

let rec build_impl args imports =
  (* TODO: file that checks the hash of the dependencies *)
  (* TODO: We don't nececary need .sfw in the .sfwi contains only types *)
  (* TODO: Memoize *)
  let aux (gamma_acc, impl_acc) modul =
    let interface = build_intf imports in
    let file = impl_of_module modul in
    let impl =
      compile
        ~interface
        {args with print = false; lto = false; file}
    in
    match impl_acc with
    | Some impl_acc ->
        (Gamma.union (modul, interface) gamma_acc, Some (Backend.link impl impl_acc))
    | None ->
        (Gamma.union (modul, interface) gamma_acc, Some impl)
  in
  let (gamma, impl) = List.fold_left aux (Gamma.empty, None) imports in
  (gamma, impl)

and compile ~interface args =
  let (imports, parse_tree) = parse args.file Parser.main in
  let (gamma, code) = build_impl args imports in
  let dst =
    TypeChecker.from_parse_tree gamma parse_tree
    |> Lambda.of_typed_tree
    (* TODO: Give an the modules to be initialized *)
    |> Backend.make ~with_main:(Gamma.is_empty interface) ~name:(module_of_file args.file)
    |> Backend.optimize ~lto:args.lto ~opt:args.opt
  in
  match code with
  | Some code -> Backend.link dst code
  | None -> dst

let compile = compile ~interface:Gamma.empty
