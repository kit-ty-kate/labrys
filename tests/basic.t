Init

  $ cp $CERVOISE/tests/Basic.sfw .

Normal compilation

  $ $CERVOISE/main.native build-program Basic
  Compiling Basic
  Linking Basic

Test compiling printed LLVM-IR code

  $ $CERVOISE/main.native print-early-llvm Basic | llc-3.7 - -o /dev/null
  Linking Basic
