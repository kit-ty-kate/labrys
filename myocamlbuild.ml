open Ocamlbuild_pkg

let subst_files = [
  "src/config.ml"
]

let substs = [
  ("%%LIBDIR%%", Ocamlbuild_plugin.run_and_read "opam config var lib");
  ("%%VERSION%%", Ocamlbuild_plugin.run_and_read "grep '^version:' opam | cut -d '\"' -f 2");
]

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    Substs.dispatcher subst_files substs hook;
    Pkg.dispatcher
      {
        Pkg.pkg_name = "cervoise";
        Pkg.lib = None;
        Pkg.bins = [
          ("src/main", Some "cervoise");
        ];
        Pkg.files = [
          Install.files "lib" [
            Install.file ~check:`NoCheck "stdlib/Prelude.sfwi";
            Install.file ~check:`NoCheck "dest/stdlib/Prelude.bc";
            Install.file ~check:`NoCheck "dest/stdlib/Prelude.csfw";
          ]
        ];
      }
      hook;
  )
