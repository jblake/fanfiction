#!/bin/bash

sqlite3 /srv/tags/tags.db "select item from all_items" | while read STORY; do
  ./steal.sh "${STORY}" "$@"
  echo
done

if [ -e BROKEN ]; then
  echo "Broken stories:"
  cat BROKEN
  rm BROKEN
  echo
fi

beep
