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

type file = string
type directory = string

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
end

class type modul = object
  inherit base_llvm
end

class type print_parse_tree = object
  inherit restrained_base
end

class type print_unsugared_tree = object
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
