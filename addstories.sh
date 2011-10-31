#!/bin/bash

set -e

while [ $# -gt 0 ]; do

  STORY="$1"
  shift 1

  export TMPDIR="${PWD}"

  PAGE="$(tempfile -p ff_ -s .html)"

  function cleanTemps () {
    rm -f "${PAGE}"
  }

  trap cleanTemps EXIT

  echo -n "Fetching ${STORY}: "
  until wget -q -O "${PAGE}" "http://m.fanfiction.net/s/${STORY}/1"; do echo -n "!"; done
  echo -n ". "

  SHIPTAGS="$(grep -m1 'Rated: ' "${PAGE}" | perl -pne 'if ( /,  ([^,]+),/ ) { $_ = $1; y/A-Z/a-z/; @tags = split( /&/ ); foreach ( @tags ) { s/[^a-zA-Z0-9]+/_/g; s/^_+|_+$//g }; $_ = join( ", ", @tags ) } else { $_ = "noship" }')"

  echo "${SHIPTAGS}"
  echo "${STORY}: ${SHIPTAGS}" >> STORIES

  cleanTemps
  trap - EXIT

done

tagcoll copy -g STORIES | sponge STORIES
