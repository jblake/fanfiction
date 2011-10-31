#!/bin/bash

read SEARCH

cd import

tagcoll grep --derived=../DERIVED --items "${SEARCH}" ../STORIES | while read STORY; do
  ls *"_${STORY}.epub"
done
