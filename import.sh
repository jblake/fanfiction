#!/bin/sh

set -e

./build.sh

psql -f schema.sql fanfiction fanfiction

./ff import
