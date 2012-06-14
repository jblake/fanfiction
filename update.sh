#!/bin/sh

set -e

./build.sh

dist/build/update/update +RTS -N
