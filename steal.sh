#!/bin/bash

set -e

STORY="$1"
shift 1

FORCE=false

while [ $# -gt 0 ]; do
  if [ "$1" == "-f" ]; then
    FORCE=true
  else
    echo "Unexpected option $1"
    exit 1
  fi
  shift 1
done

export TMPDIR="${PWD}"

if [ "${STORY}" == "" ]; then
  echo "You must provide a story ID!"
  exit 1
fi

declare -a CHAPTERS

EPUB="$(tempfile -p ff_ -s .epub)"

function cleanTemps () {
  echo "Cleaning up..."
  rm -f "${CHAPTERS[@]}" "${EPUB}"
}

trap cleanTemps EXIT

function getOneChapter () {

  CHAPTER="$1"
  shift 1

  TEMP="$(tempfile -p ff_ -s .html)"

  CHAPTERS[${#CHAPTERS[@]}]="${TEMP}"

  until wget -q -O "${TEMP}" "http://m.fanfiction.net/s/${STORY}/${CHAPTER}"; do echo -n "!"; done

  echo -n "."

}

function getAllChapters () {

  if grep -m1 -q '>Next &#187;</a>' "${TEMP}"; then
    getOneChapter "$((${CHAPTER} + 1))"
    getAllChapters
  fi

}

if egrep -q "^${STORY}:.* prune(,|$)" STORIES; then
  echo "${STORY}: Pruned."
  rm -f import/*_"${STORY}".epub
else

  echo -n "${STORY}: Fetching first chapter... "
  getOneChapter 1
  echo

  TITLE="$(grep -m1 'by <a href=' "${CHAPTERS[0]}" | perl -pne 's/.*<b>(.*?)<\/b>.*/$1/; s/&(?!(#[0-9]+;)|([a-z]+;))/&amp;/g')"
  AUTHOR="$(grep -m1 'by <a href=' "${CHAPTERS[0]}" | perl -pne 's/.*>(.*?)<\/a>.*/$1/; s/&(?!(#[0-9]+;)|([a-z]+;))/&amp;/g')"
  DATE="$(date -d "$(grep -m1 Rated: "${CHAPTERS[0]}" | perl -pne 's/.* (U|P):([^<]+?)<.*/$2/' | tr - /)")"

  if [ "${TITLE}" == "" ]; then
    echo "This story (${STORY}) doesn't appear to exist!"
  else

    echo "This is ${TITLE} by ${AUTHOR}, updated ${DATE}."
    MTITLE="$(echo "${TITLE}" | perl -pne 's/[^a-zA-Z0-9]+/_/g' | perl -pne 's/(^_)|(_$)//g')_by_$(echo "${AUTHOR}" | perl -pne 's/[^a-zA-Z0-9]+/_/g' | perl -pne 's/(^_)|(_$)//g')_${STORY}"
    echo "I'm calling it ${MTITLE}."

    touch -d "${DATE}" "${CHAPTERS[0]}"

    if "${FORCE}" || [ ! -e "import/${MTITLE}.epub" ] || [ "${CHAPTERS[0]}" -nt "import/${MTITLE}.epub" ]; then

      rm -f import/*_"${STORY}".epub

      if grep -q 'script-attribute-c.png' "${CHAPTERS[0]}"; then
        echo "${STORY}: +complete" >> STORIES.patch
      fi

      echo -n "Fetching remaining chapters... "
      getAllChapters
      echo

      echo "Building EPUB..."
      ./mkepub.sh "${EPUB}" "${STORY}" "${TITLE}" "${AUTHOR}" "${CHAPTERS[@]}"

      echo "Copying to reader..."
      cp "${EPUB}" "import/${MTITLE}.epub"

      touch -d "${DATE}" "import/${MTITLE}.epub"

      echo "Success!"

    fi

  fi

fi
