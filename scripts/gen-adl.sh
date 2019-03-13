#!/bin/bash
set -ex

SCRIPT_DIR="$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT=$SCRIPT_DIR/..
ADLC=$SCRIPT_DIR/adlc
ADLSTDLIB="$(${ADLC} show --adlstdlib)"
rm -rf $ROOT/src/ADL
${ADLC} haskell -O $ROOT/src --package=ADL --rtpackage=ADL.Core --include-rt -I $ROOT/adl $ROOT/adl/types.adl $ROOT/adl/config.adl $ROOT/adl/release.adl $ROOT/adl/state.adl $ROOT/adl/nginx.adl $ADLSTDLIB/sys/types.adl
