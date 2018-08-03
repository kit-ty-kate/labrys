Normal compilation

  $ $LABRYS build-program Nat
  Compiling Nat
  Linking Nat
  $ $LABRYS build-program TestImports
  Compiling TestImports
  Compiling Multi
  Compiling Test.Lol
  Compiling Test.Multi
  Linking TestImports
  $ $LABRYS build-program YetAnotherBug
  Compiling YetAnotherBug
  Linking YetAnotherBug
  $ $LABRYS build-program Bug
  Compiling Bug
  Linking Bug
  $ $LABRYS build-program CombS
  Compiling CombS
  Linking CombS
  $ $LABRYS build-program Exn
  Compiling Exn
  Error in 'Exn.sfw' from line 9 column 4 to line 9 column 8:
      Effects are not allowed on toplevel
--> exit 1
  $ $LABRYS build-program Fact
  Compiling Fact
  Linking Fact
  $ $LABRYS build-program TailFact
  Compiling TailFact
  Linking TailFact
  $ $LABRYS build-program NativeFact
  Compiling NativeFact
  Linking NativeFact
  $ $LABRYS build-program GrosGrosBug
  Compiling GrosGrosBug
  Linking GrosGrosBug
  $ $LABRYS build-program LetRecIn
  Compiling LetRecIn
  Linking LetRecIn
  $ $LABRYS build-program MultiTypes
  Compiling MultiTypes
  Linking MultiTypes
  $ $LABRYS build-program Print
  Compiling Print
  Linking Print
  $ $LABRYS build-program Rec
  Compiling Rec
  Linking Rec
  $ $LABRYS build-program Simple
  Compiling Simple
  Linking Simple
  $ $LABRYS build-program SystemFOmega
  Compiling SystemFOmega
  Linking SystemFOmega
  $ $LABRYS build-program SystemF
  Compiling SystemF
  Linking SystemF
  $ $LABRYS build-program Test
  Compiling Test
  Linking Test
  $ $LABRYS build-program UselessEnv
  Compiling UselessEnv
  Linking UselessEnv
  $ $LABRYS build-program Variants
  Compiling Variants
  Linking Variants
  $ $LABRYS build-program ExnVar
  Compiling ExnVar
  Linking ExnVar
  $ $LABRYS build-program Lol
  Compiling Lol
  Linking Lol

Test compiling printed LLVM-IR code

  $ $LABRYS print-early-llvm Nat | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Nat
  $ $LABRYS print-early-llvm TestImports | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking TestImports
  $ $LABRYS print-early-llvm YetAnotherBug | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking YetAnotherBug
  $ $LABRYS print-early-llvm Bug | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Bug
  $ $LABRYS print-early-llvm CombS | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking CombS
  $ $LABRYS print-early-llvm Exn | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Compiling Exn
  Error in 'Exn.sfw' from line 9 column 4 to line 9 column 8:
      Effects are not allowed on toplevel
  $ $LABRYS print-early-llvm Fact | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Fact
  $ $LABRYS print-early-llvm TailFact | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking TailFact
  $ $LABRYS print-early-llvm NativeFact | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking NativeFact
  $ $LABRYS print-early-llvm GrosGrosBug | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking GrosGrosBug
  $ $LABRYS print-early-llvm LetRecIn | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking LetRecIn
  $ $LABRYS print-early-llvm MultiTypes | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking MultiTypes
  $ $LABRYS print-early-llvm Print | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Print
  $ $LABRYS print-early-llvm Rec | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Rec
  $ $LABRYS print-early-llvm Simple | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Simple
  $ $LABRYS print-early-llvm SystemFOmega | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking SystemFOmega
  $ $LABRYS print-early-llvm SystemF | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking SystemF
  $ $LABRYS print-early-llvm Test | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Test
  $ $LABRYS print-early-llvm UselessEnv | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking UselessEnv
  $ $LABRYS print-early-llvm Variants | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Variants
  $ $LABRYS print-early-llvm ExnVar | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking ExnVar
  $ $LABRYS print-early-llvm Lol | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Lol
