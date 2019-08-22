#!/bin/bash

#Emulate infrastructure-as-code setting up machine's config

echo "Emulating infrastructure-as-code steps"
TESTROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null && pwd )"

S3EMULATE="${TESTROOT}/s3"
MACHINE_ROOT="${TESTROOT}/machine"



echo "Emulating Setting up machine with hx-deploy-tool"
mkdir -p ${MACHINE_ROOT}/opt/bin
mkdir -p ${MACHINE_ROOT}/opt/etc

cp ${TESTROOT}/../hx-deploy-tool ${MACHINE_ROOT}/opt/bin/hx-deploy-tool

echo "Emulating writing hx-deploy-tool config"

# also solves problem that hx-deploy-tool needs absolute paths in there
cat ./tool-config.json | sed -r -e "s|--MACHINE_ROOT--|${MACHINE_ROOT}|g" | sed -r -e "s|--S3EMULATE--|${S3EMULATE}|g" > ${MACHINE_ROOT}/opt/etc/hx-deploy-tool.json 
