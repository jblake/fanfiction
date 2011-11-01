#!/bin/bash

for f in $(./find.sh unread | rl); do

  n="$(echo $f | perl -pne 's/.*_([0-9]+)\.epub$/$1/')"

  #fbreader import/$f 2>/dev/null
  echo $f

  ./tag.sh $n

done
