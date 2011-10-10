#!/bin/sh

cat /dev/stdin STORIES | sort -u | sponge STORIES
