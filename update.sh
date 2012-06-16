#!/bin/sh

set -e

./build.sh

until ls /media/nook/ > /dev/null 2>&1; do file -s /dev/sdc > /dev/null 2>&1 ; sleep 0.1; done

rsync --delete --inplace --recursive --times --verbose --existing /srv/epubs/ "/media/nook/My Files/Books/fanfiction/"

time -p dist/build/update/update +RTS -N -RTS "$@"

rsync --delete --inplace --recursive --times --verbose /srv/epubs/ "/media/nook/My Files/Books/fanfiction/"
