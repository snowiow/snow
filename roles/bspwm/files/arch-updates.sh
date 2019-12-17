#!/bin/sh

updates="$(checkupdates | wc -l)"

if [ -z "$updates" ]; then
    echo "0"
else
    echo $updates
fi
