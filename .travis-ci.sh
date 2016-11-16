# Init OPAM bin directory
mkdir -p $HOME/opam-bin
export PATH="$HOME/opam-bin:$PATH"

# Install & Init OPAM if not cached
if type opam; then
    cp -r $HOME/opam-cache $HOME/.opam
else
    # Install & Init OPAM
    wget "https://github.com/ocaml/opam/releases/download/1.2.2/opam-1.2.2-x86_64-Linux" -O $HOME/opam-bin/opam
    chmod u+x $HOME/opam-bin/opam
    opam init -y --compiler=$OCAML_VERSION

    # Save for cache
    cp -r $HOME/.opam $HOME/opam-cache
fi

eval `opam config env`

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
make tests

# Uninstall
opam remove -y cervoise
