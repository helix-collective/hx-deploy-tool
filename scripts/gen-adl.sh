#!/bin/bash
set -ex

SCRIPT_DIR="$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPO_ROOT="$SCRIPT_DIR"/..

if [ -z "${ADLC-}" ]; then
    # Setup ADL from stack:
    if [ "${NIX_PATH-}" ]; then
        STACK="stack --nix"
    else
        STACK="stack"
    fi

    (cd $REPO_ROOT; $STACK setup 2>/dev/null && $STACK build adl-compiler)
    export ADLC=$(cd $REPO_ROOT; $STACK path --local-install-root)/bin/adlc
fi

ROOT="${REPO_ROOT}"
ADLSTDLIB="$(${ADLC} show --adlstdlib)"
rm -rf $ROOT/src/ADL
${ADLC} haskell -O $REPO_ROOT/src --package=ADL --rtpackage=ADL.Core --include-rt -I $ROOT/adl $ROOT/adl/types.adl $ROOT/adl/config.adl $ROOT/adl/release.adl $ROOT/adl/state.adl $ADLSTDLIB/sys/types.adl

rm -rf $REPO_ROOT/typescript
${ADLC} typescript -O $REPO_ROOT/typescript/ --include-rt --runtime-dir runtime -I $ROOT/adl $ROOT/adl/types.adl $ROOT/adl/config.adl $ADLSTDLIB/sys/types.adl
# Add an empty tslint config to prevent lint errors from generated code.
echo '{}' > $REPO_ROOT/typescript/tslint.json
