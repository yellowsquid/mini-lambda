#!/usr/bin/bash

if ! test -d "$1"; then
    echo "Needs a directory containing the test."
    exit 1
fi

function find_no_suffix {
    find "$1" -name "*.$2" | sed -E "s/.$2\$//" | sort
}

ROOT=$(basename "$1")
SOURCE="$1/$ROOT.lambda"
CASES=$(comm -12 <(find_no_suffix "$1" 'in') <(find_no_suffix "$1" 'out'))

TMP=$(mktemp -d)
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

if ! env "$DIR/lambda" -o "$TMP/exec" "$SOURCE"; then
    echo "Failed to compile source."
    rm -rf "$TMP"
    exit 2
fi

MAX_INTERP=6

for c in $CASES ; do
    for (( i=0; i <= MAX_INTERP; i++ )) ; do
        if diff <(env "$DIR/lambda" -i "$i" "$SOURCE" <"$c.in") "$c.out"; then
            echo "PASS: $c-$i"
        else
            echo "FAIL: $c-$i"
            rm -rf "$TMP"
            exit 3
        fi
    done

    if diff <("$TMP/exec" <"$c.in") "$c.out"; then
        echo "PASS: $c-c"
    else
        echo "FAIL: $c-c"
        rm -rf "$TMP"
        exit 4
    fi
done

rm -rf "$TMP"
