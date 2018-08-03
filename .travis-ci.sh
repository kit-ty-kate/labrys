cat << EOF > Dockerfile
FROM ocaml/opam2:debian-unstable
ADD . /home/opam/labrys
WORKDIR /home/opam/labrys
RUN sudo chown -R opam:opam .
RUN opam switch $OCAML_VERSION

# Install Ubuntu packages
RUN echo "deb http://llvm.org/apt/unstable/ llvm-toolchain-$LLVM_VERSION main" | sudo tee -a /etc/apt/sources.list
RUN curl -L http://llvm.org/apt/llvm-snapshot.gpg.key | sudo apt-key add -
RUN sudo apt-get update -qq
RUN sudo apt-get install -qq cmake "llvm-$LLVM_VERSION" libgc-dev

# Check OPAM package description
RUN opam lint --warn=-48-21-32 *.opam

# Install & tests
RUN opam repository set-url default https://opam.ocaml.org/2.0
RUN opam pin add -y --no-action --kind=git labrys .
RUN opam pin add -y --no-action --kind=version llvm "${LLVM_VERSION}${LLVM_VERSION_MICRO}"
RUN opam install -y opam-depext
RUN opam depext -y labrys
RUN opam install -yt --deps-only labrys
RUN opam install -yvt labrys
RUN eval \$(opam env) && make tests
EOF
docker build .
