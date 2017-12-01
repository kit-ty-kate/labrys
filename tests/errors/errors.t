Init

  $ export CERVOISE_LIBDIR=$CERVOISE/stdlib
  $ cp $CERVOISE/tests/errors/InvalidRec.sfw .

Normal compilation

  $ $CERVOISE/main.native build-module InvalidRec
  Compiling InvalidRec
  Error in 'InvalidRec.sfw' from line 2 column 13 to line 2 column 14:
      This recursive value cannot be used here
  [1]
