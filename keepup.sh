#!/bin/bash

for STORY in $(cat STORIES); do
  ./steal.sh "${STORY}" "$@"
  echo
done

ls -hl  "/media/nook/My Files/Books/fanfiction" > index-name
ls -hlS "/media/nook/My Files/Books/fanfiction" > index-size
ls -hlt "/media/nook/My Files/Books/fanfiction" > index-date

beep
