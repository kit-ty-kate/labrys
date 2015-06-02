sudo add-apt-repository --yes ppa:avsm/ocaml42+opam12
sudo add-apt-repository --yes ppa:ubuntu-toolchain-r/test
echo "deb http://llvm.org/apt/precise/ llvm-toolchain-precise-$LLVM_VERSION main" | sudo tee -a /etc/apt/sources.list
wget -O - http://llvm.org/apt/llvm-snapshot.gpg.key | sudo apt-key add -
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
sudo apt-get install -qq "llvm-$LLVM_VERSION" libgc-dev clang
export OPAMYES=1
opam init
eval `opam config env`
opam pin add -n --kind=git cervoise .
opam install "llvm.$LLVM_VERSION" cervoise.dev

cervoise build-program --src-dir examples TailFact
./a.out
