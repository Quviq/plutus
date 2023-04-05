#!/bin/bash

Prelude=pure-cake/src/PureCake/PureCakePrelude.hs
Impl=pure-cake/src/PureCake/Implementation.hs
Tests=TestCases.hs
Test=pure-cake/src/PureCake/PureCakeTestHarness.hs

pure=$PURECAKE/examples/lib/pure
cake=$PURECAKE/examples/lib/cake
ffi=$PURECAKE/examples/lib/basis_ffi.o

function preprocess() {
  cat $1 | gsed -e '/PURECAKE START/,/PURECAKE STOP/!d'
}

function unaryMinus() {
  cat $1 | gsed -e 's/-\([0-9]\)/~\1/g'
}

(cat $Prelude ; preprocess $Impl; unaryMinus $Tests ; preprocess $Test) \
  | $pure | $cake --skip_type_inference=true --exclude_prelude=true --sexp=true > cek.S

if [ $? == 0 ]; then
  gcc -arch x86_64 cek.S $ffi -o purecek
  rm cek.S
  ./purecek
fi
