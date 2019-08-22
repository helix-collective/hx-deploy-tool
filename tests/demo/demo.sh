#!/bin/bash

echo "Emulating hx-deploy-tool basics"

echo "Emulating dev and infra steps"
(cd development; ./dev.sh; ./infra.sh)

echo "Emulating launch of a deploy on a machine"
(cd machine; cd opt/bin; ./hx-deploy-tool start release-xx-yy-zz.zip)



