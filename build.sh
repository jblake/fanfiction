#!/bin/sh

set -e

cabal configure -O2 --disable-executable-profiling
cabal build

sudo svc -t /etc/service/webtag/
