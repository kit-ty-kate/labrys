Init

  $ cd $OLD_PWD/tests

Normal compilation

  $ ../main.native build-program Basic
  Compiling Basic
  Linking Basic

Test compiling printed LLVM-IR code

  $ ../main.native print-early-llvm Basic | llc-3.7 - -o /dev/null
  Linking Basic

Cleaning

  $ rm -rf dest
  $ rm -f a.out
