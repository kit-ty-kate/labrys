(* Copyright (c) 2013-2017 The Cervoise developers. *)
(* See the LICENSE file at the top-level directory. *)

type file = string
type directory = string
type program_name = string

class type restrained_base = object
  method src_dir : directory
  method build_dir : directory
end

class type base = object
  inherit restrained_base
  method lib_dir : directory
  method no_prelude : bool
end

class type base_llvm = object
  inherit base
  method debug : bool
end

class type optimization = object
  method lto : bool
  method opt : int
end

class type program = object
  inherit base_llvm
  inherit optimization
  method o : file
  method cc : program_name
end

class type modul = object
  inherit base_llvm
end

class type print_parse_tree = object
  inherit restrained_base
end

class type print_desugared_tree = object
  inherit base
end

class type print_pretyped_tree = object
  inherit base
end

class type print_untyped_tree = object
  inherit base
end

class type print_lambda_tree = object
  inherit base
end

class type print_optimized_tree = object
  inherit base
end

class type print_early_llvm = object
  inherit base_llvm
end

class type print_llvm = object
  inherit base_llvm
  inherit optimization
end
