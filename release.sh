#!/usr/bin/env bash
# Copyright (c) 2020 The DAML Authors. All rights reserved.
# SPDX-License-Identifier: Apache-2.0


set -euo pipefail

CURRENT=$(cat LATEST | awk '{print $2}')
STABLE_REGEX='\d\+\.\d\+\.\d\+'
VERSION_REGEX="^${STABLE_REGEX}\(-alpha\.\d\{8\}\.\d\+\.[0-9a-f]\{8\}\)\?$"

if ! echo $CURRENT | grep -q $VERSION_REGEX; then
    echo "Invalid version number in LATEST file, needs manual correction."
    exit 1
fi


is_stable () {
    local version="$1"
    echo "$version" | grep -q "^${STABLE_REGEX}$"
}

make_snapshot() {
    local sha=$1
    local commit_date=$(git log -n1 --format=%cd --date=format:%Y%m%d $sha)
    local number_of_commits=$(git rev-list --count $sha)
    local commit_sha_8=$(git log -n1 --format=%h --abbrev=8 $sha)
    local prerelease="alpha.$commit_date.$number_of_commits.$commit_sha_8"
    if is_stable "$CURRENT"; then
        local stable="$CURRENT"
    else
        local stable=$(echo "$CURRENT" | grep -o "^$STABLE_REGEX")
    fi
    echo "$sha $stable-$prerelease" > LATEST
    echo "Updated LATEST file."
}

display_help() {
    cat <<EOF
This script is meant to help with managing the LATEST file. Usage:

$0 snapshot SHA
        Updates the LATEST file to point to the given SHA (which must be a
        valid git reference to a commit on origin/master). If the current
        version defined in LATEST is already a snapshot, keeps the stable part
        of the version unchanged; otherwise, increments the patch number.

Any other invocation will display this help message.

Note: at the moment, changing the version string for a stable release is left
as a manual exercice, but that may change in the future.
EOF
}

case $1 in
    snapshot)
        git fetch origin master 1>/dev/null 2>&1
        if [ -n "${2+x}" ] && git merge-base --is-ancestor $2 origin/master >/dev/null; then
            make_snapshot $(git rev-parse $2)
        else
            display_help
        fi
        ;;
    *)
        display_help
        ;;
esac

