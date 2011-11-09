#!/bin/bash

echo "Waiting for nook..."
until ls nook/ > /dev/null 2>&1; do sleep 0.1; done

rsync --delete --inplace --recursive --times --verbose import/ nook/fanfiction/
