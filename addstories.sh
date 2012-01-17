#!/bin/bash

set -e

export TMPDIR="${PWD}"

while [ $# -gt 0 ]; do

  STORY="$(perl -e '$ARGV[0] =~ s#^http://(m|www)\.fanfiction\.net/s/##; $ARGV[0] =~ s#/.*$##; print "$ARGV[0]\n";' "$1")"
  shift 1

  if grep -q "^${STORY}:" STORIES; then
    echo "Already have ${STORY}."
  else

    PAGE="$(tempfile -p ff_ -s .html)"

    function cleanTemps () {
      rm -f "${PAGE}"
    }

    trap cleanTemps EXIT

    echo -n "Fetching ${STORY}: "
    until wget -q -O "${PAGE}" "http://m.fanfiction.net/s/${STORY}/1"; do echo -n "!"; done
    echo -n ". "

    SHIPTAGS="$(grep -m1 'Rated: ' "${PAGE}" | perl -pne 'if ( /,  ([^,]+),/ ) { $_ = $1; y/A-Z/a-z/; @tags = split( /&/ ); foreach ( @tags ) { s/[^a-zA-Z0-9]+/_/g; s/^_+|_+$//g }; $_ = join( ", ", @tags ) } else { $_ = "noship" }')"

    if grep -q 'script-attribute-c.png' "${PAGE}"; then
      SHIPTAGS="${SHIPTAGS}, complete"
    fi

    echo "${SHIPTAGS}"
    echo "${STORY}: ${SHIPTAGS}, unread" >> STORIES

    cleanTemps
    trap - EXIT

    ./steal.sh "${STORY}"

  fi

done

tagcoll copy -g STORIES | sponge STORIES

git commit STORIES -m "Adding stories."
