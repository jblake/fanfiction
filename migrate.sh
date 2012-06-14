#!/bin/sh

set -e

pg_dump -a -F custom -f migrate.dump -U fanfiction fanfiction
psql -f schema.sql fanfiction fanfiction
pg_restore -a -F custom -f migrate.dump -U fanfiction -d fanfiction

rm -f migrate.dump
