Init

  $ cp $CERVOISE/tests/basic/Basic.sfw .

Normal compilation

  $ $CERVOISE/main.native build-program Basic
  Compiling Basic
  Linking Basic

Test compiling printed LLVM-IR code

  $ $CERVOISE/main.native print-early-llvm Basic | llc-$LLVM_VERSION - -o /dev/null
  Linking Basic
