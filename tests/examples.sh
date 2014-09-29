#!/bin/sh

cd examples

EXAMPLES=`ls *.sfw`

for x in $EXAMPLES; do
    ../main.native $x;
done

echo
echo Test compiling printed llvm code
echo

for x in $EXAMPLES; do
    ../main.native --print-early-llvm $x | llc-3.5 - -o test.s
done
