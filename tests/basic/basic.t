Normal compilation

  $ $LABRYS build-program Basic
  Compiling Basic
  Linking Basic

Test compiling printed LLVM-IR code

  $ $LABRYS print-early-llvm Basic | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Basic
