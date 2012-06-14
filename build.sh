#!/bin/sh

set -e

cabal configure -O2
cabal build
