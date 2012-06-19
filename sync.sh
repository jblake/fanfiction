#!/bin/sh

set -e

until ls "/media/nook/My Files" > /dev/null 2>&1; do file -s /dev/sdc > /dev/null 2>&1 || true; sleep 0.1; done

rsync --delete --inplace --recursive --times --verbose --existing /srv/epubs/ "/media/nook/My Files/Books/fanfiction/"

time -p ./ff update +RTS -N -RTS "$@"

rsync --delete --inplace --recursive --times --verbose /srv/epubs/ "/media/nook/My Files/Books/fanfiction/"

sudo umount /media/nook
