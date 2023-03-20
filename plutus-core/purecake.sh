#!/bin/bash

Prelude=pure-cake/src/PureCake/PureCakePrelude.hs
Impl=pure-cake/src/PureCake/Implementation.hs

pure=$PURECAKE/examples/lib/pure
cake=$PURECAKE/examples/lib/cake

(cat $Prelude ; (cat $Impl | gsed -e '/PURECAKE START/,/PURECAKE STOP/!d')) \
  | $pure | $cake --skip_type_inference=true --exclude_prelude=true --sexp=true > cek.S
