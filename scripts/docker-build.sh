#!/bin/bash

# Helper script to build under docker, for linux deployment.
# Write a compressed binary to /tmp/hx-deploy-tool.gz

set -ex

USER=$(whoami)
USERID=$(id -u)
GROUPID=$(id -g)

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/..


stack docker pull
stack --docker build
EXE=$(stack --docker exec which hx-deploy-tool)
echo EXE $EXE
cp $EXE /tmp/hx-deploy-tool.$(arch)-linux 
gzip -f /tmp/hx-deploy-tool.$(arch)-linux

