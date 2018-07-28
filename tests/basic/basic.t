Normal compilation

  $ $CERVOISE build-program Basic
  Compiling Basic
  Linking Basic

Test compiling printed LLVM-IR code

  $ $CERVOISE print-early-llvm Basic | llc-$LLVM_VERSION - -o /dev/null
  Linking Basic
