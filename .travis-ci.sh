cat << EOF > Dockerfile
FROM ocaml/opam2:$DISTRO
ADD . /home/opam/labrys
WORKDIR /home/opam/labrys
RUN sudo chown -R opam:opam .
RUN opam switch $OCAML_VERSION

# Check OPAM package description
RUN opam lint --warn=-48-21-32 *.opam

# Install & tests
RUN opam repository set-url default https://opam.ocaml.org/
RUN opam pin add -y --no-action --kind=git labrys .
RUN opam pin add -y --no-action --kind=version llvm ${LLVM_VERSION}
RUN opam install -y opam-depext
RUN opam depext -uy labrys
RUN opam install -y --deps-only labrys
RUN opam install -yvt labrys
RUN eval \$(opam env) && make tests
EOF
docker build .
