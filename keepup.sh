#!/bin/bash

echo "Waiting for nook..."
until ls import/ > /dev/null 2>&1; do sleep 0.1; done

for STORY in $(cat STORIES); do
  ./steal.sh "${STORY}" "$@"
  echo
done

find "import/fanfiction" -type f -printf '%P\n' | sort > index-name
find "import/fanfiction" -type f -printf '%s %P\n' | sort -nr > index-size
find "import/fanfiction" -type f -printf '%TY-%Tm-%Td %P\n' | sort -r > index-date

beep
