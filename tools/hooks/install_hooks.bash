#!/usr/bin/env bash

set -o errexit

# Grab the directory of _this_ script.
SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECTDIR="$(realpath "${SCRIPTDIR}"/../..)"

# Try to link each hook into the project's .git/hooks directory.
for hook in $(ls "${SCRIPTDIR}"/*.hook); do
    stub="${hook%.*}"
    dest="${PROJECTDIR}/.git/hooks/$(basename ${stub})"
    ln -sv "${hook}" "${dest}"
done
