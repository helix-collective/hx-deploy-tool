#!/bin/bash
set -e
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/..
rm -rf $ROOT/src/ADL
adlc haskell -O $ROOT/src --package=ADL --rtpackage=ADL.Core --include-rt -I $ROOT/adl $ROOT/adl/types.adl $ROOT/adl/config.adl $ROOT/adl/release.adl
