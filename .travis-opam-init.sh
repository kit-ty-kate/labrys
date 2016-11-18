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
fi

# Save for cache
eval `opam config env`
opam update
rm -rf $HOME/opam-cache
cp -r $HOME/.opam $HOME/opam-cache
