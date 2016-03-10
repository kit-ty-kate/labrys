Init

  $ cp $CERVOISE/examples/Hello.sfw .
  $ cp $CERVOISE/examples/Fact.sfw .
  $ cp $CERVOISE/examples/TailFact.sfw .
  $ cp $CERVOISE/examples/NativeFact.sfw .
  $ cp $CERVOISE/examples/Nat.sfw .
  $ cp $CERVOISE/examples/Nat.sfwi .

Normal compilation

  $ $CERVOISE/main.native build-program Hello
  Compiling Hello
  Linking Hello
  $ $CERVOISE/main.native build-program Fact
  Compiling Fact
  Compiling Nat
  Linking Fact
  $ $CERVOISE/main.native build-program TailFact
  Compiling TailFact
  Linking TailFact
  $ $CERVOISE/main.native build-program NativeFact
  Compiling NativeFact
  Linking NativeFact

Test compiling printed LLVM-IR code

  $ $CERVOISE/main.native print-early-llvm Hello | llc-3.8 - -o /dev/null
  Linking Hello
  $ $CERVOISE/main.native print-early-llvm Fact | llc-3.8 - -o /dev/null
  Linking Fact
  $ $CERVOISE/main.native print-early-llvm TailFact | llc-3.8 - -o /dev/null
  Linking TailFact
  $ $CERVOISE/main.native print-early-llvm NativeFact | llc-3.8 - -o /dev/null
  Linking NativeFact

Test behaviours

  $ $CERVOISE/main.native build-program Hello
  Linking Hello
  $ ./a.out
  Hello World !
  $ $CERVOISE/main.native build-program Fact
  Linking Fact
  $ ./a.out
  1
  1
  2
  6
  24
  120
  362880
  $ $CERVOISE/main.native build-program TailFact
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
  $ $CERVOISE/main.native build-program NativeFact
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
