#!/bin/sh

set -e

cabal configure -O2
cabal build

sudo svc -t /etc/service/webtag/
