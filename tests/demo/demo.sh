#!/bin/bash

echo "Emulating camus2 basics"

echo "Emulating dev and infra steps"
(cd development; ./dev.sh; ./infra.sh)

echo "Emulating launch of a deploy on a machine"
(cd machine; cd opt/bin; ./c2 start release-xx-yy-zz.zip)



