Normal compilation

  $ labrys build-program Nat
  Compiling Nat
  Linking Nat
  $ labrys build-program TestImports
  Compiling TestImports
  Compiling Multi
  Compiling Test.Lol
  Compiling Test.Multi
  Linking TestImports
  $ labrys build-program YetAnotherBug
  Compiling YetAnotherBug
  Linking YetAnotherBug
  $ labrys build-program Bug
  Compiling Bug
  Linking Bug
  $ labrys build-program CombS
  Compiling CombS
  Linking CombS
  $ labrys build-program Exn
  Compiling Exn
  Error in 'Exn.sfw' from line 9 column 4 to line 9 column 8:
      Effects are not allowed on toplevel
  [1]
  $ labrys build-program Fact
  Compiling Fact
  Linking Fact
  $ labrys build-program TailFact
  Compiling TailFact
  Linking TailFact
  $ labrys build-program NativeFact
  Compiling NativeFact
  Linking NativeFact
  $ labrys build-program GrosGrosBug
  Compiling GrosGrosBug
  Linking GrosGrosBug
  $ labrys build-program LetRecIn
  Compiling LetRecIn
  Linking LetRecIn
  $ labrys build-program MultiTypes
  Compiling MultiTypes
  Linking MultiTypes
  $ labrys build-program Print
  Compiling Print
  Linking Print
  $ labrys build-program Rec
  Compiling Rec
  Linking Rec
  $ labrys build-program Simple
  Compiling Simple
  Linking Simple
  $ labrys build-program SystemFOmega
  Compiling SystemFOmega
  Linking SystemFOmega
  $ labrys build-program SystemF
  Compiling SystemF
  Linking SystemF
  $ labrys build-program Test
  Compiling Test
  Linking Test
  $ labrys build-program UselessEnv
  Compiling UselessEnv
  Linking UselessEnv
  $ labrys build-program Variants
  Compiling Variants
  Linking Variants
  $ labrys build-program ExnVar
  Compiling ExnVar
  Linking ExnVar
  $ labrys build-program Lol
  Compiling Lol
  Linking Lol

Test compiling printed LLVM-IR code

  $ labrys print-early-llvm Nat | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Nat
  $ labrys print-early-llvm TestImports | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking TestImports
  $ labrys print-early-llvm YetAnotherBug | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking YetAnotherBug
  $ labrys print-early-llvm Bug | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Bug
  $ labrys print-early-llvm CombS | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking CombS
  $ labrys print-early-llvm Exn | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Compiling Exn
  Error in 'Exn.sfw' from line 9 column 4 to line 9 column 8:
      Effects are not allowed on toplevel
  $ labrys print-early-llvm Fact | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Fact
  $ labrys print-early-llvm TailFact | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking TailFact
  $ labrys print-early-llvm NativeFact | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking NativeFact
  $ labrys print-early-llvm GrosGrosBug | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking GrosGrosBug
  $ labrys print-early-llvm LetRecIn | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking LetRecIn
  $ labrys print-early-llvm MultiTypes | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking MultiTypes
  $ labrys print-early-llvm Print | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Print
  $ labrys print-early-llvm Rec | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Rec
  $ labrys print-early-llvm Simple | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Simple
  $ labrys print-early-llvm SystemFOmega | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking SystemFOmega
  $ labrys print-early-llvm SystemF | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking SystemF
  $ labrys print-early-llvm Test | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Test
  $ labrys print-early-llvm UselessEnv | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking UselessEnv
  $ labrys print-early-llvm Variants | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Variants
  $ labrys print-early-llvm ExnVar | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking ExnVar
  $ labrys print-early-llvm Lol | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Lol
