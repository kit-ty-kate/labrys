Normal compilation

  $ labrys build-program Nat.sfw
  Compiling Nat
  Linking Nat
  $ labrys build-program TestImports.sfw
  Compiling TestImports
  Compiling Multi
  Compiling Test.Lol
  Compiling Test.Multi
  Linking TestImports
  $ labrys build-program YetAnotherBug.sfw
  Compiling YetAnotherBug
  Linking YetAnotherBug
  $ labrys build-program Bug.sfw
  Compiling Bug
  Linking Bug
  $ labrys build-program CombS.sfw
  Compiling CombS
  Linking CombS
  $ labrys build-program Exn.sfw
  Compiling Exn
  Error in 'Exn.sfw' from line 9 column 4 to line 9 column 8:
      Effects are not allowed on toplevel
  [1]
  $ labrys build-program Fact.sfw
  Compiling Fact
  Linking Fact
  $ labrys build-program TailFact.sfw
  Compiling TailFact
  Linking TailFact
  $ labrys build-program NativeFact.sfw
  Compiling NativeFact
  Linking NativeFact
  $ labrys build-program GrosGrosBug.sfw
  Compiling GrosGrosBug
  Linking GrosGrosBug
  $ labrys build-program LetRecIn.sfw
  Compiling LetRecIn
  Linking LetRecIn
  $ labrys build-program MultiTypes.sfw
  Compiling MultiTypes
  Linking MultiTypes
  $ labrys build-program Print.sfw
  Compiling Print
  Linking Print
  $ labrys build-program Rec.sfw
  Compiling Rec
  Linking Rec
  $ labrys build-program Simple.sfw
  Compiling Simple
  Linking Simple
  $ labrys build-program SystemFOmega.sfw
  Compiling SystemFOmega
  Linking SystemFOmega
  $ labrys build-program SystemF.sfw
  Compiling SystemF
  Linking SystemF
  $ labrys build-program Test.sfw
  Compiling Test
  Linking Test
  $ labrys build-program UselessEnv.sfw
  Compiling UselessEnv
  Linking UselessEnv
  $ labrys build-program Variants.sfw
  Compiling Variants
  Linking Variants
  $ labrys build-program ExnVar.sfw
  Compiling ExnVar
  Linking ExnVar
  $ labrys build-program Lol.sfw
  Compiling Lol
  Linking Lol

Test compiling printed LLVM-IR code

  $ labrys print-early-llvm Nat.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Nat
  $ labrys print-early-llvm TestImports.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking TestImports
  $ labrys print-early-llvm YetAnotherBug.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking YetAnotherBug
  $ labrys print-early-llvm Bug.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Bug
  $ labrys print-early-llvm CombS.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking CombS
  $ labrys print-early-llvm Exn.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Compiling Exn
  Error in 'Exn.sfw' from line 9 column 4 to line 9 column 8:
      Effects are not allowed on toplevel
  $ labrys print-early-llvm Fact.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Fact
  $ labrys print-early-llvm TailFact.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking TailFact
  $ labrys print-early-llvm NativeFact.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking NativeFact
  $ labrys print-early-llvm GrosGrosBug.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking GrosGrosBug
  $ labrys print-early-llvm LetRecIn.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking LetRecIn
  $ labrys print-early-llvm MultiTypes.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking MultiTypes
  $ labrys print-early-llvm Print.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Print
  $ labrys print-early-llvm Rec.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Rec
  $ labrys print-early-llvm Simple.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Simple
  $ labrys print-early-llvm SystemFOmega.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking SystemFOmega
  $ labrys print-early-llvm SystemF.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking SystemF
  $ labrys print-early-llvm Test.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Test
  $ labrys print-early-llvm UselessEnv.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking UselessEnv
  $ labrys print-early-llvm Variants.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Variants
  $ labrys print-early-llvm ExnVar.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking ExnVar
  $ labrys print-early-llvm Lol.sfw | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Lol
