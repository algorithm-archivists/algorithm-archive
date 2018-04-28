#!/usr/bin/env bash

if [ $# -ge 1 ]; then
    if [ $# -eq 2 ]; then
        out_dir="$2"
    else
        if [ -z ${AAA_DIR+x} ]; then
            echo "No output directory specified"
            exit 1
        fi
        out_dir="$AAA_DIR"
    fi

    if [[ ! -d "$out_dir" ]]; then
        echo "$out_dir is not a directory"
        exit 1
    fi

    gitbook build -o /tmp/algorithm-archivists.github.io

    if [ $? -ne 0 ]; then
        echo "Failed to build the book"
        exit 1
    fi

    cd "$out_dir"
    git reset --hard && \
      git clean -dfx && \
      git pull

    if [ $? -ne 0 ]; then
        echo "Failed to prepare repository"
        exit 1
    fi

    cp /tmp/algorithm-archivists.github.io/* .
    rm -r /tmp/algorithm-archivists.github.io
    git add .
    git commit -m "$1"
    git push upstream master

else
    echo "No commit message specified"
fi
