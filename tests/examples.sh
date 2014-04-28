#!/bin/sh

EXAMPLES=`ls examples/*.sfw`

for x in $EXAMPLES; do
    echo "File: $x"
    ./main.native $x;
done
