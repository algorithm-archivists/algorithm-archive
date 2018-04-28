#!/bin/sh

if [ $# -eq 1 ]; then
    gitbook build -o /tmp/algorithm-archivists.github.io
    cp /tmp/algorithm-archivists.github.io/* ../algorithm-archivists.github.io
    rm -r /tmp/algorithm-archivists.github.io
    cd ../algorithm-archivists.github.io
    git add .
    git commit -m "$1"
    git push upstream master
fi
