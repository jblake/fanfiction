#!/bin/bash

while [ $# -gt 0 ]; do

  STORY="$1"
  shift 1

  while true; do

    grep "^${STORY}" STORIES
    read -p "?> " PATCH

    if [ "${PATCH}" == "" ]; then
      break
    fi

    echo "${STORY}: ${PATCH}" | tagcoll copy -g -p /dev/stdin STORIES | sponge STORIES

  done

done
