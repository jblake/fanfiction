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

  if grep -q '>Next &#187;</a>' "${TEMP}"; then
    getOneChapter "$((${CHAPTER} + 1))"
    getAllChapters
  fi

}

echo -n "Fetching first chapter... "
getOneChapter 1
echo

TITLE="$(grep 'by <a href=' "${CHAPTERS[0]}" | perl -pne 's/.*<b>(.*?)<\/b>.*/$1/')"
AUTHOR="$(grep 'by <a href=' "${CHAPTERS[0]}" | perl -pne 's/.*>(.*?)<\/a>.*/$1/')"
DATE="$(date -d "$(grep Rated: "${CHAPTERS[0]}" | perl -pne 's/.* (U|P):([^<]+?)<.*/$2/' | tr - /)")"
echo "This is ${TITLE} by ${AUTHOR}, updated ${DATE}."
MTITLE="$(echo "${TITLE}" | perl -pne 's/[^a-zA-Z0-9]+/_/g' | perl -pne 's/(^_)|(_$)//g')_by_$(echo "${AUTHOR}" | perl -pne 's/[^a-zA-Z0-9]+/_/g' | perl -pne 's/(^_)|(_$)//g')_${STORY}"
echo "I'm calling it ${MTITLE}."

touch -d "${DATE}" "${CHAPTERS[0]}"

if "${FORCE}" || [ ! -e "/media/nook/My Files/Books/fanfiction/${MTITLE}.epub" ] || [ "${CHAPTERS[0]}" -nt "/media/nook/My Files/Books/fanfiction/${MTITLE}.epub" ]; then

  echo -n "Fetching remaining chapters... "
  getAllChapters
  echo

  echo "Building EPUB..."
  ./mkepub.sh "${EPUB}" "${STORY}" "${TITLE}" "${AUTHOR}" "${CHAPTERS[@]}"

  echo "Copying to reader..."
  mkdir -p "/media/nook/My Files/Books/fanfiction"
  cp "${EPUB}" "/media/nook/My Files/Books/fanfiction/${MTITLE}.epub"

  touch -d "${DATE}" "/media/nook/My Files/Books/fanfiction/${MTITLE}.epub"

  echo "Success!"

else
  echo "Already on reader."
fi
