#!/bin/sh

set -e

cabal configure
cabal build

psql -f schema.sql fanfiction fanfiction

dist/build/import/import
