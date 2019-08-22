#!/bin/bash

cd "$( dirname "${BASH_SOURCE[0]}" )"

echo "Emulating preparation and packaging of release zips"

for dirname in $(find . -type d -name 'release*'); do
# zip with release.json at root inside the zip (not in a subdir)
(cd ${dirname}; zip -r ../${dirname}.zip *)
done

echo "Emulating upload of release zips"
mv -f *.zip ../../s3/releaseZips/
