Normal compilation

  $ labrys build-program Basic.sfw
  Compiling Basic
  Linking Basic

Test compiling printed LLVM-IR code

  $ labrys print-early-llvm Basic.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Basic
