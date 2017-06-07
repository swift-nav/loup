#!/usr/bin/env bash

while [ 1 ]
do
    echo "-------------------- cwd"
    echo "$PWD"
    echo "-------------------- ls"
    ls "$PWD"
    echo "-------------------- input.json"
    cat "input.json"
    echo
    sleep 5
done
