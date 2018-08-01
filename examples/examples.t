Normal compilation

  $ $CERVOISE build-program Hello
  Compiling Hello
  Linking Hello
  $ $CERVOISE build-program Fact
  Compiling Fact
  Compiling Nat
  Linking Fact
  $ $CERVOISE build-program TailFact
  Compiling TailFact
  Linking TailFact
  $ $CERVOISE build-program NativeFact
  Compiling NativeFact
  Linking NativeFact
  $ $CERVOISE build-program PolyFact
  Compiling PolyFact
  Linking PolyFact

Test compiling printed LLVM-IR code

  $ $CERVOISE print-early-llvm Hello | llc-$LLVM_VERSION - -o /dev/null
  Linking Hello
  $ $CERVOISE print-early-llvm Fact | llc-$LLVM_VERSION - -o /dev/null
  Linking Fact
  $ $CERVOISE print-early-llvm TailFact | llc-$LLVM_VERSION - -o /dev/null
  Linking TailFact
  $ $CERVOISE print-early-llvm NativeFact | llc-$LLVM_VERSION - -o /dev/null
  Linking NativeFact
  $ $CERVOISE print-early-llvm PolyFact | llc-$LLVM_VERSION - -o /dev/null
  Linking PolyFact

Test behaviours

  $ $CERVOISE build-program Hello
  Linking Hello
  $ ./a.out
  Hello World !
  $ $CERVOISE build-program Fact
  Linking Fact
  $ ./a.out
  1
  1
  2
  6
  24
  120
  362880
  $ $CERVOISE build-program TailFact
  Linking TailFact
  $ ./a.out
  1
  1
  2
  6
  24
  120
  720
  5040
  40320
  362880
  3628800
  $ $CERVOISE build-program NativeFact
  Linking NativeFact
  $ ./a.out
  1
  1
  2
  6
  24
  120
  720
  5040
  40320
  362880
  3628800
  39916800
  $ $CERVOISE build-program PolyFact
  Linking PolyFact
  $ ./a.out
  1
  1
  2
  6
  24
  120
  720
  5040
  40320
  362880
