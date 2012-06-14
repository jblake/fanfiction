#!/bin/sh

set -e

./build.sh

psql -f schema.sql fanfiction fanfiction

dist/build/import/import
