#!/bin/sh

set -e

if [ ! -e migrate.dump ]; then
  pg_dump -a -F custom -f migrate.dump -U fanfiction fanfiction
fi

psql -f schema.sql fanfiction fanfiction
pg_restore -a -F custom -U fanfiction -d fanfiction migrate.dump
psql -c "vacuum full analyze" fanfiction fanfiction

rm -f migrate.dump
