#!/bin/bash

#Emulate infrastructure-as-code setting up machine's config

echo "Emulating infrastructure-as-code steps"
TESTROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null && pwd )"

S3EMULATE="${TESTROOT}/s3"
MACHINE_ROOT="${TESTROOT}/machine"



echo "Emulating Setting up machine with camus2"
mkdir -p ${MACHINE_ROOT}/opt/bin
mkdir -p ${MACHINE_ROOT}/opt/etc

cp ${TESTROOT}/../c2 ${MACHINE_ROOT}/opt/bin/c2

echo "Emulating writing camus2 config"

# also solves problem that camus2 needs absolute paths in there
cat ./tool-config.json | sed -r -e "s|--MACHINE_ROOT--|${MACHINE_ROOT}|g" | sed -r -e "s|--S3EMULATE--|${S3EMULATE}|g" > ${MACHINE_ROOT}/opt/etc/camus2.json 
