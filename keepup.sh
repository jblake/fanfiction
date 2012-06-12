#!/bin/bash

./sync.sh --existing

TOTAL="$(sqlite3 /srv/tags/tags.db "select count(*) from all_items")"
N=0

sqlite3 /srv/tags/tags.db "select item from all_items" | while read STORY; do
  ./steal.sh "${STORY}" "$@"
  echo
  echo "$N" "$TOTAL" | dbar
  echo
  N="$(($N + 1))"
done

if [ -e BROKEN ]; then
  echo "Broken stories:"
  (
    cd import
    cat ../BROKEN | while read STORY; do
      if [ -e *"_${STORY}.epub" ]; then
        echo *"_${STORY}.epub"
      else
        echo "${STORY}"
      fi
    done
  )
  rm BROKEN
  echo
fi

./sync.sh

beep
