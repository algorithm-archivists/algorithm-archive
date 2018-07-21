#!/usr/bin/env bash

# update_site_travis.bash: Update a GitHub repo with the book build
# products.

set -o errexit

# Required environment variables:
# - DOCS_BRANCH_NAME: name of the remote branch serving the book
# - GH_REPO_NAME: name of the remote repo
# - GH_USER: name of the remote repo's owner
# - GH_TOKEN: [secret] Personal Access Token

if [ -z ${DOCS_BRANCH_NAME+x} ]; then
    echo "\$DOCS_BRANCH_NAME is not set!"
    exit 1
elif [ -z ${GH_REPO_NAME+x} ]; then
    echo "\$GH_REPO_NAME is not set!"
    exit 1
elif [ -z ${GH_TOKEN+x} ]; then
    echo "\$GH_TOKEN is not set!"
    exit 1
elif [ -z ${GH_USER+x} ]; then
    echo "\$GH_USER is not set!"
    exit 1
fi

git config user.name "Travis CI User"
git config user.email "travis@travis-ci.org"

GH_REPO_REF="github.com/${GH_USER}/${GH_REPO_NAME}.git"

# Assume the book has already been built, and was moved to `build/`
# inside the same directory as this script.

if [ -d build ]; then
    echo "Cloning the website repo..."
    git clone -b $DOCS_BRANCH_NAME https://git@${GH_REPO_REF}
    rm -rf ./"${GH_REPO_NAME}"/*
    cp -a ./build/* ./"${GH_REPO_NAME}"
    pushd ./"${GH_REPO_NAME}"
    echo "Adding changes..."
    git add --all
    echo "Committing..."
    # This will return 1 if there are no changes, which should not
    # result in failure.
    git commit \
        -m "Deploy book to GitHub Pages Travis build: ${TRAVIS_BUILD_NUMBER}" \
        -m "Commit: ${TRAVIS_COMMIT}" || ret=$?
    git push --force "https://${GH_TOKEN}@${GH_REPO_REF}" > /dev/null 2>&1
    popd
else
    echo "" >&2
    echo "Warning: The book wasn't found found!" >&2
    echo "Warning: Not going to push the book to GitHub!" >&2
    exit 1
fi
