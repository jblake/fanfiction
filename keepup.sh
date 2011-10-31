#!/bin/bash

while IFS=: read STORY TAGS; do
  ./steal.sh "${STORY}" "$@"
  echo
done < STORIES

beep
