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

module TT = TypedTree

open MonadOpen

let m = LLVM.create_module Types.context "Main"

let find_in_gamma name = List.find (fun x -> Unsafe.(fst x = name))

let rec lambda gammaParam gammaEnv gammaGlob builder = function
  | TT.Abs ({TT.abs_ty; TT.param; TT.ty_expr}, t) ->
      let args = [| Types.to_llvm param.TT.ty; Types.env |] in
      let ty = LLVM.function_type (Types.to_llvm ty_expr) args in
      let f = LLVM.define_function "__lambda" ty m in
      let builder = LLVM.builder_at_end Types.context (LLVM.entry_block f) in
      let gammaP = [(param.TT.name, LLVM.param f 0)] in
      let f = LLVM.build_gep f [||] "addr_f" builder in
      lambda gammaP (gammaParam @ gammaEnv) gammaGlob builder t >>= fun v ->
      ignore (LLVM.build_ret v builder);
      Exn.return (LLVM.const_struct Types.context [| f; LLVM.const_null Types.env |])
  | TT.App (ty, f, x) ->
      lambda gammaParam gammaEnv gammaGlob builder f >>= fun f ->
      let env = LLVM.build_extractvalue f 1 "env" builder in
      let f = LLVM.build_extractvalue f 0 "f" builder in
      lambda gammaParam gammaEnv gammaGlob builder x >>= fun x ->
      Exn.return (LLVM.build_call f [| x; env |] "tmp" builder)
  | TT.Val {TT.name; TT.ty} ->
      let c = ref 0 in
      let find gamma = incr c; find_in_gamma name gamma >|= snd in
      let execEnv value =
        let param = LLVM.param value 0 in
        LLVM.build_extractvalue param 0 "from_env" builder
      in
      let execGlob value =
        LLVM.const_struct Types.context [| value; LLVM.const_null Types.env |]
      in
      let res = find gammaParam in
      c := 0;
      let res = Exn.catch res (fun `NotFound -> find gammaEnv >|= execEnv) in
      c := 0;
      let res = Exn.catch res (fun `NotFound -> find gammaGlob >|= execGlob) in
      res

let rec init gammaGlob builder = function
  | (name, g, t) :: xs ->
      lambda [] [] gammaGlob builder t >>= fun value ->
      ignore (LLVM.build_store value g builder);
      init ((name, g) :: gammaGlob) builder xs
  | [] -> Exn.return ()

let print =
  let rec top init_list = function
    | TT.Value ({TT.name; TT.ty}, t) :: xs ->
        let g = LLVM.define_global name (LLVM.undef (Types.to_llvm ty)) m in
        top ((name, g, t) :: init_list) xs
    | [] ->
        let ty = LLVM.function_type (LLVM.void_type Types.context) [||] in
        let f = LLVM.define_function "__init" ty m in
        let builder = LLVM.builder_at_end Types.context (LLVM.entry_block f) in
        init [] builder (List.rev init_list) >>= fun () ->
        LLVM.dump_module m;
        Exn.return ()
  in
  top []
