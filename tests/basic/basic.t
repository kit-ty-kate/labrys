Normal compilation

  $ labrys build-program Basic.sfw
  Compiling Basic
  Linking Basic

Test compiling printed LLVM-IR code

  $ export LLC=${LLC:-$(command -v llc)}
  $ export LLC=${LLC:-$(command -v llc11)}
  $ export LLC=${LLC:-$(command -v llc-11)}
  $ labrys print-early-llvm Basic.sfw | "$LLC" - -o /dev/null
  Linking Basic
