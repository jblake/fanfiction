#!/bin/bash

while true; do

  STORY="$(sqlite3 /srv/tags/tags.db "select item from tags t1 where tag = 'unread' and not exists (select * from tags t2 where t1.item = t2.item and t2.tag = 'prune') order by random() limit 1")"

  FILE="$(cd import; ls *_"${STORY}".epub)"

  clear
  echo "${FILE}"
  echo
  sqlite3 /srv/tags/tags.db "select tag from tags where item = '${STORY}' order by tag asc"
  echo
  read

done
