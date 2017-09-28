(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

open UntypedTree

let check ~modul ~interface ~with_main options env x =
  assert false (* TODO *)

let check_interface ~current_module options =
  let aux env = function
    | PretypedTree.IVal (name, ty) ->
        Env.add_toplevel_value name ty env
    | PretypedTree.IAbstractType (name, k) ->
        Env.add_abstract_type name k env
    | PretypedTree.IDatatype (name, k, args, variants) ->
        Env.add_datatype name k args variants env
    | PretypedTree.ITypeAlias (name, ty) ->
        Env.add_type_alias name ty env
    | PretypedTree.IException (name, args) ->
        Env.add_exception name args env
    | PretypedTree.IClass (name, args, sigs) ->
        Env.add_class name args sigs env
    | PretypedTree.IInstance (instance, name) ->
        Env.add_instance instance name env
  in
  fun env l ->
    Env.diff (List.fold_left aux env l) env
