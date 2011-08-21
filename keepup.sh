#!/bin/bash

echo "Waiting for nook..."
until ls import/ > /dev/null 2>&1; do sleep 0.1; done

for STORY in $(cat STORIES); do
  ./steal.sh "${STORY}" "$@"
  echo
done

ls -hl  "import/fanfiction" > index-name
ls -hlS "import/fanfiction" > index-size
ls -hlt "import/fanfiction" > index-date

beep
