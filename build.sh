#!/bin/bash

set -e

MODE="${1:-release}"

: "${CC:=cc}"

CFLAGS="-Isrc/thirdparty -Isrc/codegen \
        -Werror -Wpedantic -Wall -Wextra \
        -Wno-implicit-fallthrough \
        -std=c11"

if [[ "$MODE" == "debug" ]]; then
    CFLAGS="$CFLAGS -ggdb"
    OUTDIR="build/debug"
elif [[ "$MODE" == "release" ]]; then
    CFLAGS="$CFLAGS -O2 -DNDEBUG"
    OUTDIR="build/release"
else
    echo "Invalid mode: $MODE. Use 'debug' or 'release'."
    exit 1
fi

mkdir -p "$OUTDIR"
$CC $CFLAGS src/zag.c -o "$OUTDIR/zag"
