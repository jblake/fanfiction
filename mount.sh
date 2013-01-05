#!/bin/bash

set -e

mkdir -p zulu

until sshfs root@zulu.omgwallhack.org:/ zulu; do
  sleep 0.1
done
