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

  $ $CERVOISE/main.native print-early-llvm Hello | llc-3.7 - -o /dev/null
  Linking Hello
  $ $CERVOISE/main.native print-early-llvm Fact | llc-3.7 - -o /dev/null
  Linking Fact
  $ $CERVOISE/main.native print-early-llvm TailFact | llc-3.7 - -o /dev/null
  Linking TailFact
  $ $CERVOISE/main.native print-early-llvm NativeFact | llc-3.7 - -o /dev/null
  Linking NativeFact
