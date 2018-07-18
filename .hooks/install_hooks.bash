#!/usr/bin/env bash

# Grab the directory of _this_ script.
SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Try to link each hook into the project's .git/hooks directory.
for hook in $(ls "${SCRIPTDIR}"/*.hook); do
    stub="${hook%.*}"
    dest="$(realpath ${SCRIPTDIR}/../.git/hooks)/$(basename ${stub})"
    ln -sv "${hook}" "${dest}"
done
