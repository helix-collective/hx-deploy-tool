#!/bin/bash

echo "Emulating development steps"

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
cd ${DIR}/releaseSource
./zip.sh
