#!/bin/bash

# Helper script to build under docker, for linux deployment.
# Write a compressed binary to /tmp/camus2.gz

set -ex

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null && pwd )"

stack docker pull
stack --docker build
EXE=$(stack --docker exec which c2)
TARGET=/tmp/camus2.$(stack --docker exec arch)-linux 
cp $EXE $TARGET
gzip -f $TARGET
echo "Binary written to $TARGET.gz"

cp $EXE ${ROOT}/tests/
