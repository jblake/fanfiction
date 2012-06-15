#!/bin/sh

set -e

./build.sh

time -p dist/build/update/update +RTS -N -RTS "$@"
