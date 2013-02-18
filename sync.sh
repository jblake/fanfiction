#!/bin/bash

set -ex

./mount.sh

ssh root@zulu.omgwallhack.org "while pkill com.flyersoft.moonreaderp; do sleep 1; done"

time dist/build/ff/ff update

rsync --delete --inplace --recursive --times --verbose books/ realbooks/

./unmount.sh
