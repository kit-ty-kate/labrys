#use "topfind"
#require "containers"
#require "containers.unix"

let () = if Array.length Sys.argv <> 2 then assert false

let fmt = Printf.sprintf
let cmd x = CCUnix.with_process_in x ~f:CCIO.read_lines_l

let version = Sys.argv.(1)
let lib = List.hd (cmd "opam config var lib")

let l = [
  fmt "let version = \"%s\"" (String.escaped version);
  "let lib =";
  "  try Sys.getenv \"CERVOISE_LIBDIR\"";
  fmt "  with Not_found -> \"%s/cervoise\"" (String.escaped lib);
]

let () = CCIO.write_lines_l stdout l
