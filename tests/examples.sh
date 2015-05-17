#!/bin/sh

cd examples

EXAMPLES="\
    Multi \
    Nat \
    TestImports \
    YetAnotherBug \
    Bug \
    CombS \
    Exn \
    Fact \
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
"

for x in $EXAMPLES; do
    ../main.native $x;
done

echo
echo Test compiling printed llvm code
echo

for x in $EXAMPLES; do
    ../main.native --print-early-llvm $x | llc-3.5 - -o test.s
done
