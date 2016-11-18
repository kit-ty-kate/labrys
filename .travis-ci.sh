# Init OPAM
source .travis-opam-init.sh

# Install Ubuntu packages
sudo add-apt-repository --yes ppa:likemartinma/devel # cmake
sudo add-apt-repository --yes ppa:martin-frost/dev-packages # cram
sudo add-apt-repository "deb http://llvm.org/apt/trusty/ llvm-toolchain-trusty-$LLVM_VERSION main"
wget -O - http://llvm.org/apt/llvm-snapshot.gpg.key | sudo apt-key add -
sudo apt-get update -qq
sudo apt-get install -qq cmake
sudo apt-get install -qq "llvm-$LLVM_VERSION" libgc-dev clang
sudo apt-get install -qq python-cram

# Check OPAM package description
opam lint

# Install
opam pin add -y --no-action --kind=git cervoise .
opam install -y "llvm.$LLVM_VERSION" cervoise

# Run tests
make
make LLVM_VERSION="$LLVM_VERSION" tests

# Uninstall
opam remove -y cervoise
