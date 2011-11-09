#!/bin/bash

if tty -s < /dev/stdout; then
  COLUMNATE="column"
else
  COLUMNATE="cat"
fi

exec tagcoll reverse --derived=DERIVED --items STORIES | $COLUMNATE
