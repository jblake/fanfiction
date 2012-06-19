#!/bin/sh

set -e

psql -f schema.sql fanfiction fanfiction

./ff import
