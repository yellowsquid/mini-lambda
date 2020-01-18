#!/usr/bin/bash

TMP=$(mktemp -d)
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

env "$DIR/lambda" -o "$TMP/exec" "$@" && "$TMP/exec"
rm -rf "$TMP"
