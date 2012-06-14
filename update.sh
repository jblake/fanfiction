#!/bin/sh

set -e

cabal configure
cabal build

dist/build/update/update +RTS -N
