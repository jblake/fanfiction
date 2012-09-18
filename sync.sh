#!/bin/sh

set -e

file -s /dev/sdc > /dev/null 2>&1 || true
sleep 5

until ls "/media/nook/My Files" > /dev/null 2>&1; do

  file -s /dev/sdc > /dev/null 2>&1 || true

  rl -c 1 << END
Waiting for Nook to show up...
You can go ahead and plug the Nook in now.
Any time, jerk.
Why don't you plug in the Nook?
I'm not sure what you're waiting for, but I'm waiting for you.
The Nook hasn't shown up yet.
Hey! Plug in the Nook!
What's the matter? Can't find the cable?
Sitting around doing nothing is boring.
*crickets*
END

  sleep 5

done

echo

rsync --delete --inplace --recursive --times --verbose --existing /srv/epubs/ "/media/nook/My Files/Books/fanfiction/"

time -p ./ff update +RTS -N -RTS "$@"

rsync --delete --inplace --recursive --times --verbose /srv/epubs/ "/media/nook/My Files/Books/fanfiction/"

sudo umount /media/nook

beep
