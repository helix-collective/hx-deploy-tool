#!/bin/bash
set -ex
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/..
ADLSTDLIB="$(adlc show --adlstdlib)"
rm -rf $ROOT/src/ADL
adlc haskell -O $ROOT/src --package=ADL --rtpackage=ADL.Core --include-rt -I $ROOT/adl $ROOT/adl/types.adl $ROOT/adl/config.adl $ROOT/adl/release.adl $ADLSTDLIB/sys/types.adl
