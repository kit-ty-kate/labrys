#!/bin/sh

cd examples

EXAMPLES=`ls *.sfw`

for x in $EXAMPLES; do
    ../main.native $x;
done
