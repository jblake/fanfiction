#!/bin/bash

for STORY in $(cat STORIES); do
  ./steal.sh "${STORY}" "$@"
  echo
done

beep
