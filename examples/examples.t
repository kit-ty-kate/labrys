Normal compilation

  $ labrys build-program Hello
  Compiling Hello
  Linking Hello
  $ labrys build-program Fact
  Compiling Fact
  Compiling Nat
  Linking Fact
  $ labrys build-program TailFact
  Compiling TailFact
  Linking TailFact
  $ labrys build-program NativeFact
  Compiling NativeFact
  Linking NativeFact
  $ labrys build-program PolyFact
  Compiling PolyFact
  Linking PolyFact

Test compiling printed LLVM-IR code

  $ labrys print-early-llvm Hello | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Hello
  $ labrys print-early-llvm Fact | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Fact
  $ labrys print-early-llvm TailFact | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking TailFact
  $ labrys print-early-llvm NativeFact | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking NativeFact
  $ labrys print-early-llvm PolyFact | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking PolyFact

Test behaviours

  $ labrys build-program Hello
  Linking Hello
  $ ./a.out
  Hello World !
  $ labrys build-program Fact
  Linking Fact
  $ ./a.out
  1
  1
  2
  6
  24
  120
  40320
  $ labrys build-program TailFact
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
  $ labrys build-program NativeFact
  Linking NativeFact
  $ ./a.out
  1
  1
  2
  6
  24
  120
  5040
  40320
  $ labrys build-program PolyFact
  Linking PolyFact
  $ ./a.out
  1
  1
  2
  6
  24
  720
  5040
  40320
