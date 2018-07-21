#!/usr/bin/env bash

# update_site_travis.bash: Update a GitHub repo with the book build
# products.

set -o errexit

# Required environment variables:
# - DOCS_BRANCH_NAME: name of the remote branch serving the book
# - DOCS_REPO_NAME: name of the remote repo
# - DOCS_REPO_OWNER: name of the remote repo's owner
# - GH_TOKEN: [secret] Personal Access Token

bold=$(tput bold)
normal=$(tput sgr0)

if [ -z ${DOCS_BRANCH_NAME+x} ]; then
    echo "${bold}\$DOCS_BRANCH_NAME is not set!${normal}"
    exit 1
elif [ -z ${DOCS_REPO_NAME+x} ]; then
    echo "${bold}\$DOCS_REPO_NAME is not set!${normal}"
    exit 1
elif [ -z ${DOCS_REPO_OWNER+x} ]; then
    echo "${bold}\$DOCS_REPO_OWNER is not set!${normal}"
    exit 1
elif [ -z ${GH_TOKEN+x} ]; then
    echo "${bold}\$GH_TOKEN is not set!${normal}"
    exit 1
fi

git config user.name "Travis CI User"
git config user.email "travis@travis-ci.org"

GH_REPO_REF="github.com/${DOCS_REPO_OWNER}/${DOCS_REPO_NAME}.git"

# Assume the book has already been built, and was moved to `build/`
# inside the same directory as this script.

if [ -d build ]; then
    echo "${bold}Cloning the website repo...${normal}"
    git clone -b $DOCS_BRANCH_NAME https://git@${GH_REPO_REF}
    rm -rf ./"${DOCS_REPO_NAME}"/*
    cp -a ./build/* ./"${DOCS_REPO_NAME}"
    pushd ./"${DOCS_REPO_NAME}"
    # echo "www.algorithm-archive.org" > CNAME
    echo "${bold}Adding changes...${normal}"
    git add --all
    echo "${bold}Committing...${normal}"
    # This will return 1 if there are no changes, which should not
    # result in failure.
    git commit \
        -m "Deploy book to GitHub Pages Travis build: ${TRAVIS_BUILD_NUMBER}" \
        -m "Commit: ${TRAVIS_COMMIT}" || ret=$?
    git push "https://${GH_TOKEN}@${GH_REPO_REF}" > /dev/null 2>&1
    popd
else
    echo "" >&2
    echo "${bold}Warning: The book wasn't found!${normal}" >&2
    echo "${bold}Warning: Not going to push the book to GitHub!${normal}" >&2
    exit 1
fi
