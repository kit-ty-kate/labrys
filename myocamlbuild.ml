open Ocamlbuild_pkg

let subst_files = [
  "src/config.ml"
]

let cmd str =
  let str = Ocamlbuild_plugin.run_and_read str in
  let len = String.length str in
  String.sub str 0 (pred len)

let substs = [
  ("%%LIBDIR%%", cmd "opam config var lib");
  ("%%VERSION%%", cmd "grep '^version:' opam | cut -d '\"' -f 2");
]

let () =
  Dispatcher.dispatch [
    Substs.dispatcher subst_files substs;
    Pkg.dispatcher
      {
        Pkg.pkg_name = "cervoise";
        Pkg.libs = [];
        Pkg.bins = [
          {
            Pkg.Bin.main = "src/main";
            Pkg.Bin.options = [
              ("target", "cervoise");
            ];
          };
        ];
        Pkg.files = [
          Install.files "lib" [
            Install.file ~check:`NoCheck "stdlib/Prelude.sfwi";
            Install.file ~check:`NoCheck "dest/stdlib/Prelude.bc";
            Install.file ~check:`NoCheck "dest/stdlib/Prelude.csfw";
          ]
        ];
      };
  ]
