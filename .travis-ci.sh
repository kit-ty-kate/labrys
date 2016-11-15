# Install OPAM
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sudo sh -s /usr/local/bin

# Install Ubuntu packages
sudo add-apt-repository --yes ppa:ubuntu-toolchain-r/test
sudo add-apt-repository --yes ppa:martin-frost/dev-packages
sudo add-apt-repository "deb http://llvm.org/apt/trusty/ llvm-toolchain-trusty-$LLVM_VERSION main"
wget -O - http://llvm.org/apt/llvm-snapshot.gpg.key | sudo apt-key add -
sudo apt-get update -qq
sudo apt-get install -qq "llvm-$LLVM_VERSION" libgc-dev clang
sudo apt-get install -qq python-cram

# Init OPAM
opam init -y --compiler=$OCAML_VERSION
eval `opam config env`

# Check OPAM package description
opam lint

# Install
opam pin add -n --kind=git cervoise .
opam install "llvm.$LLVM_VERSION" cervoise

# Run tests
make
make tests

# Uninstall
opam remove -y cervoise
