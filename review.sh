#!/bin/bash

for f in $(./find.sh unread); do

  n="$(echo $f | perl -pne 's/.*_([0-9]+)\.epub$/$1/')"

  fbreader import/$f 2>/dev/null

  ./tag.sh $n

done
