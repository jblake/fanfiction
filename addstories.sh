#!/bin/bash

set -e

export TMPDIR="${PWD}"

while [ $# -gt 0 ]; do

  STORY="$(perl -e '$ARGV[0] =~ s#^http://(m|www)\.fanfiction\.net/s/##; $ARGV[0] =~ s#/.*$##; print "$ARGV[0]\n";' "$1")"
  shift 1

  if [[ "$(sqlite3 /srv/tags/tags.db "select count(*) from tags where item = '${STORY}'")" != 0 ]]; then
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

    TITLE="$(grep -m1 'by <a href=' "${PAGE}" | perl -pne 's/.*<b>(.*?)<\/b>.*/$1/; s/&(?!(#[0-9]+;)|([a-z]+;))/&amp;/g')"
    SHIPTAGS="$(grep -m1 'Rated: ' "${PAGE}" | perl -pne 'if ( /,  ([^,]+),/ ) { $_ = $1; y/A-Z/a-z/; @tags = split( /&/ ); foreach ( @tags ) { s/[^a-zA-Z0-9]+/_/g; s/^_+|_+$//g }; $_ = join( " ", @tags ) } else { $_ = "noship" }')"

    if [[ "${TITLE}" == "" ]]; then
      echo "Does not appear to exist!"
    else

      for TAG in ${SHIPTAGS} unread; do
        sqlite3 /srv/tags/tags.db "insert into tags ( item, tag ) values ( '${STORY}', '${TAG}' )"
      done

      cleanTemps
      trap - EXIT

      ./steal.sh "${STORY}"

    fi

  fi

  echo

done
