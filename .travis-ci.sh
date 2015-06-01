echo "yes" | sudo add-apt-repository ppa:avsm/ocaml42+opam12
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers opam
export OPAMYES=1
opam init
eval `opam config env`
opam pin add --kind=git cervoise .
