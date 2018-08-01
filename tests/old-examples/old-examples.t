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

  $ $CERVOISE print-early-llvm Nat | llc-$LLVM_VERSION - -o /dev/null
  Linking Nat
  $ $CERVOISE print-early-llvm TestImports | llc-$LLVM_VERSION - -o /dev/null
  Linking TestImports
  $ $CERVOISE print-early-llvm YetAnotherBug | llc-$LLVM_VERSION - -o /dev/null
  Linking YetAnotherBug
  $ $CERVOISE print-early-llvm Bug | llc-$LLVM_VERSION - -o /dev/null
  Linking Bug
  $ $CERVOISE print-early-llvm CombS | llc-$LLVM_VERSION - -o /dev/null
  Linking CombS
  $ $CERVOISE print-early-llvm Exn | llc-$LLVM_VERSION - -o /dev/null
  Compiling Exn
  Error in 'Exn.sfw' from line 9 column 4 to line 9 column 8:
      Effects are not allowed on toplevel
  $ $CERVOISE print-early-llvm Fact | llc-$LLVM_VERSION - -o /dev/null
  Linking Fact
  $ $CERVOISE print-early-llvm TailFact | llc-$LLVM_VERSION - -o /dev/null
  Linking TailFact
  $ $CERVOISE print-early-llvm NativeFact | llc-$LLVM_VERSION - -o /dev/null
  Linking NativeFact
  $ $CERVOISE print-early-llvm GrosGrosBug | llc-$LLVM_VERSION - -o /dev/null
  Linking GrosGrosBug
  $ $CERVOISE print-early-llvm LetRecIn | llc-$LLVM_VERSION - -o /dev/null
  Linking LetRecIn
  $ $CERVOISE print-early-llvm MultiTypes | llc-$LLVM_VERSION - -o /dev/null
  Linking MultiTypes
  $ $CERVOISE print-early-llvm Print | llc-$LLVM_VERSION - -o /dev/null
  Linking Print
  $ $CERVOISE print-early-llvm Rec | llc-$LLVM_VERSION - -o /dev/null
  Linking Rec
  $ $CERVOISE print-early-llvm Simple | llc-$LLVM_VERSION - -o /dev/null
  Linking Simple
  $ $CERVOISE print-early-llvm SystemFOmega | llc-$LLVM_VERSION - -o /dev/null
  Linking SystemFOmega
  $ $CERVOISE print-early-llvm SystemF | llc-$LLVM_VERSION - -o /dev/null
  Linking SystemF
  $ $CERVOISE print-early-llvm Test | llc-$LLVM_VERSION - -o /dev/null
  Linking Test
  $ $CERVOISE print-early-llvm UselessEnv | llc-$LLVM_VERSION - -o /dev/null
  Linking UselessEnv
  $ $CERVOISE print-early-llvm Variants | llc-$LLVM_VERSION - -o /dev/null
  Linking Variants
  $ $CERVOISE print-early-llvm ExnVar | llc-$LLVM_VERSION - -o /dev/null
  Linking ExnVar
  $ $CERVOISE print-early-llvm Lol | llc-$LLVM_VERSION - -o /dev/null
  Linking Lol
