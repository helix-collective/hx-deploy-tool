#!/bin/bash

# Helper script to build under docker, for linux deployment.
# Write a compressed binary to /tmp/hx-deploy-tool.gz

set -ex

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null && pwd )"

stack docker pull
stack --docker build
EXE=$(stack --docker exec which hx-deploy-tool)
TARGET=/tmp/hx-deploy-tool.$(stack --docker exec arch)-linux 
cp $EXE $TARGET
gzip -f $TARGET
echo "Binary written to $TARGET.gz"

cp $EXE ${ROOT}/tests/
