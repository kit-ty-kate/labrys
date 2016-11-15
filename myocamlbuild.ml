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

let bin =
  Pkg.Bin.create
    ~main:"src/main"
    ~target:"cervoise"
    ()

let cervoise =
  Pkg.create
    ~name:"cervoise"
    ~bins:[bin]
    ~files:[
      Install.dir ~dir:"lib" [
        Install.file "stdlib/Prelude.sfwi";
        Install.file "dest/stdlib/Prelude.bc";
        Install.file "dest/stdlib/Prelude.csfw";
      ];
    ]
    ()

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    Substs.dispatcher subst_files substs hook;
    Pkg.dispatcher cervoise hook;
  )
