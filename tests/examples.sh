#!/bin/sh

EXAMPLES="\
    Multi \
    Nat \
    TestImports \
    YetAnotherBug \
    Bug \
    CombS \
    Exn \
    Fact \
    TailFact \
    NativeFact \
    GrosGrosBug \
    LetRecIn \
    MultiTypes \
    Multi \
    Print \
    Rec \
    Simple \
    SystemFOmega \
    SystemF \
    Test \
    UselessEnv \
    Variants \
    TyClass \
    Presentation \
    ExOne \
    ExnVar \
    Lol \
    Hello \
"

for x in $EXAMPLES; do
    ./main.native build-program --src-dir examples $x
done

echo
echo Test compiling printed llvm code
echo

for x in $EXAMPLES; do
    ./main.native print-early-llvm --src-dir examples $x | llc-3.7 - -o /dev/null
done
