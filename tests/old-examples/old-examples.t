Normal compilation

  $ $CERVOISE build-program Nat
  Compiling Nat
  Linking Nat
  $ $CERVOISE build-program TestImports
  Compiling TestImports
  Compiling Multi
  Compiling Test.Lol
  Compiling Test.Multi
  Linking TestImports
  $ $CERVOISE build-program YetAnotherBug
  Compiling YetAnotherBug
  Linking YetAnotherBug
  $ $CERVOISE build-program Bug
  Compiling Bug
  Linking Bug
  $ $CERVOISE build-program CombS
  Compiling CombS
  Linking CombS
  $ $CERVOISE build-program Exn
  Compiling Exn
  Error in 'Exn.sfw' from line 9 column 4 to line 9 column 8:
      Effects are not allowed on toplevel
--> exit 1
  $ $CERVOISE build-program Fact
  Compiling Fact
  Linking Fact
  $ $CERVOISE build-program TailFact
  Compiling TailFact
  Linking TailFact
  $ $CERVOISE build-program NativeFact
  Compiling NativeFact
  Linking NativeFact
  $ $CERVOISE build-program GrosGrosBug
  Compiling GrosGrosBug
  Linking GrosGrosBug
  $ $CERVOISE build-program LetRecIn
  Compiling LetRecIn
  Linking LetRecIn
  $ $CERVOISE build-program MultiTypes
  Compiling MultiTypes
  Linking MultiTypes
  $ $CERVOISE build-program Print
  Compiling Print
  Linking Print
  $ $CERVOISE build-program Rec
  Compiling Rec
  Linking Rec
  $ $CERVOISE build-program Simple
  Compiling Simple
  Linking Simple
  $ $CERVOISE build-program SystemFOmega
  Compiling SystemFOmega
  Linking SystemFOmega
  $ $CERVOISE build-program SystemF
  Compiling SystemF
  Linking SystemF
  $ $CERVOISE build-program Test
  Compiling Test
  Linking Test
  $ $CERVOISE build-program UselessEnv
  Compiling UselessEnv
  Linking UselessEnv
  $ $CERVOISE build-program Variants
  Compiling Variants
  Linking Variants
  $ $CERVOISE build-program ExnVar
  Compiling ExnVar
  Linking ExnVar
  $ $CERVOISE build-program Lol
  Compiling Lol
  Linking Lol

Test compiling printed LLVM-IR code

  $ $CERVOISE print-early-llvm Nat | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Nat
  $ $CERVOISE print-early-llvm TestImports | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking TestImports
  $ $CERVOISE print-early-llvm YetAnotherBug | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking YetAnotherBug
  $ $CERVOISE print-early-llvm Bug | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Bug
  $ $CERVOISE print-early-llvm CombS | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking CombS
  $ $CERVOISE print-early-llvm Exn | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Compiling Exn
  Error in 'Exn.sfw' from line 9 column 4 to line 9 column 8:
      Effects are not allowed on toplevel
  $ $CERVOISE print-early-llvm Fact | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Fact
  $ $CERVOISE print-early-llvm TailFact | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking TailFact
  $ $CERVOISE print-early-llvm NativeFact | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking NativeFact
  $ $CERVOISE print-early-llvm GrosGrosBug | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking GrosGrosBug
  $ $CERVOISE print-early-llvm LetRecIn | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking LetRecIn
  $ $CERVOISE print-early-llvm MultiTypes | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking MultiTypes
  $ $CERVOISE print-early-llvm Print | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Print
  $ $CERVOISE print-early-llvm Rec | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Rec
  $ $CERVOISE print-early-llvm Simple | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Simple
  $ $CERVOISE print-early-llvm SystemFOmega | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking SystemFOmega
  $ $CERVOISE print-early-llvm SystemF | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking SystemF
  $ $CERVOISE print-early-llvm Test | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Test
  $ $CERVOISE print-early-llvm UselessEnv | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking UselessEnv
  $ $CERVOISE print-early-llvm Variants | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Variants
  $ $CERVOISE print-early-llvm ExnVar | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking ExnVar
  $ $CERVOISE print-early-llvm Lol | if [ $LLVM_VERSION ]; then llc-$LLVM_VERSION - -o /dev/null; fi
  Linking Lol
