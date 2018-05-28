#!/bin/bash

# Helper script to build under docker, for linux deployment.
# Write a compressed binary to /tmp/hx-deploy-tool.gz

USER=$(whoami)
USERID=$(id -u)
GROUPID=$(id -g)

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/..

docker run -it --rm \
 -v /home/$USER/.stack:/home/$USER/.stack \
 -v $ROOT:$ROOT \
 -v /etc/passwd:/etc/passwd \
 -v /etc/group:/etc/group \
 -v /tmp:/tmp \
 --user $USERID:$GROUPID \
 -w $ROOT \
 helixta/helixdev:2018-04-17 \
 /bin/bash -c "stack --no-nix build && cp `stack --no-nix exec which -- hx-deploy-tool` /tmp/hx-deploy-tool.$(arch)-linux && gzip -f /tmp/hx-deploy-tool.$(arch)-linux"
