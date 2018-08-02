cat << EOF > Dockerfile
FROM ocaml/opam2:debian-unstable
ADD .
RUN opam switch $OCAML_VERSION
RUN eval $(opam env)

# Install Ubuntu packages
RUN echo "deb http://llvm.org/apt/unstable/ llvm-toolchain-$LLVM_VERSION main" | sudo tee -a /etc/apt/sources.list
RUN curl -L http://llvm.org/apt/llvm-snapshot.gpg.key | sudo apt-key add -
RUN sudo apt-get update -qq
RUN sudo apt-get install -qq cmake "llvm-$LLVM_VERSION" libgc-dev

# Check OPAM package description
RUN opam lint *.opam

# Install & tests
RUN opam pin add -y --no-action --kind=git cervoise .
RUN opam pin add -y --no-action --kind=version llvm "${LLVM_VERSION}${LLVM_VERSION_MICRO}"
RUN opam install -y -t cervoise
EOF
docker build .
