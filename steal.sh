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

META="$(tempfile -p ff_ -s .meta)"
EPUB="$(tempfile -p ff_ -s .epub)"
LRF="$(tempfile -p ff_ -s .lrf)"

function cleanTemps () {
  echo "Cleaning up..."
  rm -f "${CHAPTERS[@]}" "${META}" "${EPUB}" "${LRF}"
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
echo "This is ${TITLE} by ${AUTHOR}."
MTITLE="$(echo "${TITLE}" | perl -pne 's/[^a-zA-Z0-9]+/_/g' | perl -pne 's/(^_)|(_$)//g')_by_$(echo "${AUTHOR}" | perl -pne 's/[^a-zA-Z0-9]+/_/g' | perl -pne 's/(^_)|(_$)//g')_${STORY}"
echo "I'm calling it ${MTITLE}."

if "${FORCE}" || [ ! -e "/media/reader/fanfiction/${MTITLE}.lrf" ]; then

  echo -n "Fetching remaining chapters... "
  getAllChapters
  echo

  cat <<END > "${META}"
<dc:creator>${AUTHOR}</dc:creator>
<dc:title>${TITLE}</dc:title>
END

  echo "Converting to EPUB using pandoc..."
  pandoc -f html -t epub -o "${EPUB}" --epub-metadata "${META}" "${CHAPTERS[@]}"

  echo "Converting to LRF using ebook-convert..."
  ebook-convert "${EPUB}" "${LRF}" > /dev/null

  echo "Copying to reader using ebook-device..."
  mkdir -p /media/reader/fanfiction
  rm -f "/media/reader/fanfiction/${MTITLE}.lrf"
  ebook-device cp "${LRF}" "prs500:/fanfiction/${MTITLE}.lrf"

  echo "Success!"

else
  echo "Already on reader."
fi
