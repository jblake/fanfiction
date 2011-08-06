#!/bin/bash

for STORY in $(cat STORIES); do
  ./steal.sh "${STORY}" "$@"
  echo
done

ls -hl  "import/fanfiction" > index-name
ls -hlS "import/fanfiction" > index-size
ls -hlt "import/fanfiction" > index-date

beep
