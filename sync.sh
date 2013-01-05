#!/bin/bash

set -ex

./mount.sh

ssh root@zulu "pkill com.flyersoft.moonreaderp || true"

time dist/build/ff/ff update

rsync --delete --inplace --recursive --times --verbose books/ realbooks/

./unmount.sh
