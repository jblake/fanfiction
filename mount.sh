#!/bin/bash

set -e

./unmount.sh > /dev/null 2>&1 || true

mkdir -p zulu

until sshfs root@zulu.omgwallhack.org:/ zulu; do
  sleep 0.1
done
