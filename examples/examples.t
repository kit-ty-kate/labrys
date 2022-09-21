Normal compilation

  $ labrys build-program Hello.sfw
  Compiling Hello
  Linking Hello
  $ labrys build-program Fact.sfw
  Compiling Fact
  Compiling Nat
  Linking Fact
  $ labrys build-program TailFact.sfw
  Compiling TailFact
  Linking TailFact
  $ labrys build-program NativeFact.sfw
  Compiling NativeFact
  Linking NativeFact
  $ labrys build-program PolyFact.sfw
  Compiling PolyFact
  Linking PolyFact

Test compiling printed LLVM-IR code

  $ export LLC=${LLC:-$(command -v llc)}
  $ export LLC=${LLC:-$(command -v llc11)}
  $ export LLC=${LLC:-$(command -v llc-11)}
  $ labrys print-early-llvm Hello.sfw | "$LLC" - -o /dev/null
  Linking Hello
  $ labrys print-early-llvm Fact.sfw | "$LLC" - -o /dev/null
  Linking Fact
  $ labrys print-early-llvm TailFact.sfw | "$LLC" - -o /dev/null
  Linking TailFact
  $ labrys print-early-llvm NativeFact.sfw | "$LLC" - -o /dev/null
  Linking NativeFact
  $ labrys print-early-llvm PolyFact.sfw | "$LLC" - -o /dev/null
  Linking PolyFact

Test behaviours

  $ labrys build-program Hello.sfw
  Linking Hello
  $ ./a.out
  Hello World !
  $ labrys build-program Fact.sfw
  Linking Fact
  $ ./a.out
  1
  1
  2
  6
  24
  120
  40320
  $ labrys build-program TailFact.sfw
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
  $ labrys build-program NativeFact.sfw
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
  $ labrys build-program PolyFact.sfw
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
