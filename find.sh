#!/bin/bash

cd import

tagcoll grep --derived=../DERIVED --items "$*" ../STORIES | while read STORY; do
  ls *"_${STORY}.epub"
done
