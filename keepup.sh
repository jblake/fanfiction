#!/bin/bash

while IFS=: read STORY TAGS; do
  ./steal.sh "${STORY}" "$@"
  echo
done < STORIES

if [ -e STORIES.patch ]; then
  tagcoll copy -g -p STORIES.patch STORIES | sponge STORIES
  rm -f STORIES.patch
fi

beep
