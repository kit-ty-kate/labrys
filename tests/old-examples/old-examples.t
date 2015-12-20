Init

  $ cp $CERVOISE/tests/old-examples/Multi.sfw .
  $ cp $CERVOISE/tests/old-examples/Multi.sfwi .
  $ cp $CERVOISE/tests/old-examples/Nat.sfw .
  $ cp $CERVOISE/tests/old-examples/Nat.sfwi .
  $ cp $CERVOISE/tests/old-examples/TestImports.sfw .
  $ cp $CERVOISE/tests/old-examples/YetAnotherBug.sfw .
  $ cp $CERVOISE/tests/old-examples/Bug.sfw .
  $ cp $CERVOISE/tests/old-examples/CombS.sfw .
  $ cp $CERVOISE/tests/old-examples/Exn.sfw .
  $ cp $CERVOISE/tests/old-examples/Fact.sfw .
  $ cp $CERVOISE/tests/old-examples/TailFact.sfw .
  $ cp $CERVOISE/tests/old-examples/NativeFact.sfw .
  $ cp $CERVOISE/tests/old-examples/GrosGrosBug.sfw .
  $ cp $CERVOISE/tests/old-examples/LetRecIn.sfw .
  $ cp $CERVOISE/tests/old-examples/MultiTypes.sfw .
  $ cp $CERVOISE/tests/old-examples/Multi.sfw .
  $ cp $CERVOISE/tests/old-examples/Print.sfw .
  $ cp $CERVOISE/tests/old-examples/Rec.sfw .
  $ cp $CERVOISE/tests/old-examples/Simple.sfw .
  $ cp $CERVOISE/tests/old-examples/SystemFOmega.sfw .
  $ cp $CERVOISE/tests/old-examples/SystemF.sfw .
  $ cp $CERVOISE/tests/old-examples/Test.sfw .
  $ cp $CERVOISE/tests/old-examples/UselessEnv.sfw .
  $ cp $CERVOISE/tests/old-examples/Variants.sfw .
  $ cp $CERVOISE/tests/old-examples/TyClass.sfw .
  $ cp $CERVOISE/tests/old-examples/Presentation.sfw .
  $ cp $CERVOISE/tests/old-examples/ExOne.sfw .
  $ cp $CERVOISE/tests/old-examples/ExnVar.sfw .
  $ cp $CERVOISE/tests/old-examples/Lol.sfw .
  $ mkdir Test
  $ cp $CERVOISE/tests/old-examples/Test/Multi.sfw Test
  $ cp $CERVOISE/tests/old-examples/Test/Multi.sfwi Test
  $ cp $CERVOISE/tests/old-examples/Test/Lol.sfw Test
  $ cp $CERVOISE/tests/old-examples/Test/Lol.sfwi Test

Normal compilation

  $ $CERVOISE/main.native build-program Multi
  Compiling Multi
  Linking Multi
  $ $CERVOISE/main.native build-program Nat
  Compiling Nat
  Linking Nat
  $ $CERVOISE/main.native build-program TestImports
  Compiling TestImports
  Compiling Test.Lol
  Compiling Test.Multi
  Linking TestImports
  $ $CERVOISE/main.native build-program YetAnotherBug
  Compiling YetAnotherBug
  Linking YetAnotherBug
  $ $CERVOISE/main.native build-program Bug
  Compiling Bug
  Linking Bug
  $ $CERVOISE/main.native build-program CombS
  Compiling CombS
  Linking CombS
  $ $CERVOISE/main.native build-program Exn
  Compiling Exn
  Error in 'Exn.sfw' from line 9 column 4 to line 9 column 8:
      Effects are not allowed on toplevel
  [1]
  $ $CERVOISE/main.native build-program Fact
  Compiling Fact
  Linking Fact
  $ $CERVOISE/main.native build-program TailFact
  Compiling TailFact
  Linking TailFact
  $ $CERVOISE/main.native build-program NativeFact
  Compiling NativeFact
  Linking NativeFact
  $ $CERVOISE/main.native build-program GrosGrosBug
  Compiling GrosGrosBug
  Linking GrosGrosBug
  $ $CERVOISE/main.native build-program LetRecIn
  Compiling LetRecIn
  Linking LetRecIn
  $ $CERVOISE/main.native build-program MultiTypes
  Compiling MultiTypes
  Linking MultiTypes
  $ $CERVOISE/main.native build-program Multi
  Linking Multi
  $ $CERVOISE/main.native build-program Print
  Compiling Print
  Linking Print
  $ $CERVOISE/main.native build-program Rec
  Compiling Rec
  Linking Rec
  $ $CERVOISE/main.native build-program Simple
  Compiling Simple
  Linking Simple
  $ $CERVOISE/main.native build-program SystemFOmega
  Compiling SystemFOmega
  Linking SystemFOmega
  $ $CERVOISE/main.native build-program SystemF
  Compiling SystemF
  Linking SystemF
  $ $CERVOISE/main.native build-program Test
  Compiling Test
  Linking Test
  $ $CERVOISE/main.native build-program UselessEnv
  Compiling UselessEnv
  Linking UselessEnv
  $ $CERVOISE/main.native build-program Variants
  Compiling Variants
  Linking Variants
  $ $CERVOISE/main.native build-program TyClass
  Compiling TyClass
  Linking TyClass
  $ $CERVOISE/main.native build-program Presentation
  Compiling Presentation
  Linking Presentation
  $ $CERVOISE/main.native build-program ExOne
  Compiling ExOne
  Linking ExOne
  $ $CERVOISE/main.native build-program ExnVar
  Compiling ExnVar
  Linking ExnVar
  $ $CERVOISE/main.native build-program Lol
  Compiling Lol
  Linking Lol

Test compiling printed LLVM-IR code

  $ $CERVOISE/main.native print-early-llvm Multi | llc-3.7 - -o /dev/null
  Linking Multi
  $ $CERVOISE/main.native print-early-llvm Nat | llc-3.7 - -o /dev/null
  Linking Nat
  $ $CERVOISE/main.native print-early-llvm TestImports | llc-3.7 - -o /dev/null
  Linking TestImports
  $ $CERVOISE/main.native print-early-llvm YetAnotherBug | llc-3.7 - -o /dev/null
  Linking YetAnotherBug
  $ $CERVOISE/main.native print-early-llvm Bug | llc-3.7 - -o /dev/null
  Linking Bug
  $ $CERVOISE/main.native print-early-llvm CombS | llc-3.7 - -o /dev/null
  Linking CombS
  $ $CERVOISE/main.native print-early-llvm Exn | llc-3.7 - -o /dev/null
  Compiling Exn
  Error in 'Exn.sfw' from line 9 column 4 to line 9 column 8:
      Effects are not allowed on toplevel
  $ $CERVOISE/main.native print-early-llvm Fact | llc-3.7 - -o /dev/null
  Linking Fact
  $ $CERVOISE/main.native print-early-llvm TailFact | llc-3.7 - -o /dev/null
  Linking TailFact
  $ $CERVOISE/main.native print-early-llvm NativeFact | llc-3.7 - -o /dev/null
  Linking NativeFact
  $ $CERVOISE/main.native print-early-llvm GrosGrosBug | llc-3.7 - -o /dev/null
  Linking GrosGrosBug
  $ $CERVOISE/main.native print-early-llvm LetRecIn | llc-3.7 - -o /dev/null
  Linking LetRecIn
  $ $CERVOISE/main.native print-early-llvm MultiTypes | llc-3.7 - -o /dev/null
  Linking MultiTypes
  $ $CERVOISE/main.native print-early-llvm Multi | llc-3.7 - -o /dev/null
  Linking Multi
  $ $CERVOISE/main.native print-early-llvm Print | llc-3.7 - -o /dev/null
  Linking Print
  $ $CERVOISE/main.native print-early-llvm Rec | llc-3.7 - -o /dev/null
  Linking Rec
  $ $CERVOISE/main.native print-early-llvm Simple | llc-3.7 - -o /dev/null
  Linking Simple
  $ $CERVOISE/main.native print-early-llvm SystemFOmega | llc-3.7 - -o /dev/null
  Linking SystemFOmega
  $ $CERVOISE/main.native print-early-llvm SystemF | llc-3.7 - -o /dev/null
  Linking SystemF
  $ $CERVOISE/main.native print-early-llvm Test | llc-3.7 - -o /dev/null
  Linking Test
  $ $CERVOISE/main.native print-early-llvm UselessEnv | llc-3.7 - -o /dev/null
  Linking UselessEnv
  $ $CERVOISE/main.native print-early-llvm Variants | llc-3.7 - -o /dev/null
  Linking Variants
  $ $CERVOISE/main.native print-early-llvm TyClass | llc-3.7 - -o /dev/null
  Linking TyClass
  $ $CERVOISE/main.native print-early-llvm Presentation | llc-3.7 - -o /dev/null
  Linking Presentation
  $ $CERVOISE/main.native print-early-llvm ExOne | llc-3.7 - -o /dev/null
  Linking ExOne
  $ $CERVOISE/main.native print-early-llvm ExnVar | llc-3.7 - -o /dev/null
  Linking ExnVar
  $ $CERVOISE/main.native print-early-llvm Lol | llc-3.7 - -o /dev/null
  Linking Lol
